(*
 * Copyright (C) 2012-2013 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

open Bigarray
open Gnt
open OUnit

module Init = struct
	let gntalloc_path = "/dev/xen/gntalloc"

	let sanity_check () =
		if Sys.file_exists gntalloc_path
		then print_endline "Found gntalloc device - continuing with tests."
		else begin
			print_endline
				"Could not find gntalloc device - are you running Xen and a 3.x kernel?";
			exit 1
		end
end

module Helpers = struct
	let (|>) x f = f x

	let get_random_bigarray length =
		let data = Array1.create Bigarray.char Bigarray.c_layout length in
		for position = 0 to (length - 1) do
			let c = Char.chr (Random.int 256) in
			Array1.set data position c
		done;
		data
end

module Globals = struct
	open Helpers

	let domid =
		try
			let chan = Unix.open_process_in "xenstore-read domid" in
			int_of_string (input_line chan)
		with _ -> 0

	let page_size = Xenmmap.getpagesize ()
end

module Tests = struct
	(* Test opening and closing the gntshr interface. *)
	let gntshr_interface () =
		let handle = Gntshr.interface_open () in
		Gntshr.interface_close handle

	(* Test opening and closing the gnttab interface. *)
	let gnttab_interface () =
		let handle = Gnttab.interface_open () in
		Gnttab.interface_close handle

	(* Check that sharing a specified number of pages returns that number of
	 * grant references, and a Bigarray mapping the correct amount of memory. *)
	let gntshr_size_check page_count =
		let handle = Gntshr.interface_open () in
		let share = Gntshr.share_pages_exn handle Globals.domid page_count false in
		assert_equal ~msg:"Number of grant references should equal number of pages requested"
			(List.length share.Gntshr.refs) page_count;
		let contents = share.Gntshr.mapping in
		assert_equal ~msg:"Length of mapped memory should equal (page size * number of pages)"
			(Array1.dim contents) (page_count * Globals.page_size);
		Gntshr.munmap_exn handle share;
		Gntshr.interface_close handle

	(* Check that a shared page or pages can be mapped by gnttab, and that the
	 * shared memory contains the expected data. *)
	let share_data page_count =
		(* Open interfaces. *)
		let handle_gs = Gntshr.interface_open () in
		let handle_gt = Gnttab.interface_open () in
		(* Share pages. *)
		let share = Gntshr.share_pages_exn handle_gs Globals.domid page_count false in
		let share_contents = share.Gntshr.mapping in
		(* Fill the memory map with random data. *)
		let random_data = Helpers.get_random_bigarray (page_count * Globals.page_size) in
		Array1.blit random_data share_contents;
		(* Map the shared pages. *)
		let grants = List.map
			(fun reference -> {
				Gnttab.domid = Globals.domid;
				Gnttab.ref = reference
			})
			share.Gntshr.refs
		in
		match Gnttab.mapv handle_gt grants false with
		| None -> assert_failure "Failed to map memory"
		| Some mapping -> begin
			(* Check that the mapped memory contains the expected data. *)
			let mapping_contents = Gnttab.Local_mapping.to_buf mapping in
			for position = 0 to (Array1.dim random_data) - 1 do
				let original_char = Array1.get share_contents position in
				let mapped_char = Array1.get mapping_contents position in
				if original_char <> mapped_char then begin
					let error = Printf.sprintf
						"Mapped data did not match original data: position = %d, original = %c, mapped = %c"
						position original_char mapped_char
					in
					assert_failure error
				end
			done
		end;
		(* Unmap memory. *)
		Gnttab.unmap_exn handle_gt mapping;
		Gntshr.munmap_exn handle_gs share;
		(* Close interfaces. *)
		Gnttab.interface_close handle_gt;
		Gntshr.interface_close handle_gs

	let base_suite =
		"base_suite" >:::
			[
				"gntshr_interface" >:: gntshr_interface;
				"gnttab_interface" >:: gnttab_interface;
				"gntshr_single_page_size_check" >:: (fun () -> gntshr_size_check 1);
				"gntshr_multi_page_size_check" >:: (fun () -> gntshr_size_check 5);
				"single_page_share_data" >:: (fun () -> share_data 1);
				"multi_page_share_data" >:: (fun () -> share_data 7);
			]
end

let _ =
	Init.sanity_check ();
	Random.self_init();
	Printf.printf "Assuming I am running in domid %d\n%!" Globals.domid;
	run_test_tt_main Tests.base_suite
