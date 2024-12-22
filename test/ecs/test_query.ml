open Util
open Ecs

let test_filter_matches () =
  let open Query.Filter in
  let a, b, c, d = (Foo.C.id, Bar.C.id, Baz.C.id, Name.C.id) in
  assert (matches (With a) (Id.ComponentSet.of_list [ b; a; c ]));
  assert (not (matches (With a) (Id.ComponentSet.of_list [ b ])));
  assert (not (matches (With a) (Id.ComponentSet.of_list [ c ])));

  assert (matches (Without b) (Id.ComponentSet.of_list []));
  assert (not (matches (Without c) (Id.ComponentSet.of_list [ c; b; a ])));
  assert (matches (Without a) (Id.ComponentSet.of_list [ b; c; d ]));

  assert (matches (Not (With a)) (Id.ComponentSet.of_list [ b ]));
  assert (not (matches (Not (With a)) (Id.ComponentSet.of_list [ a ])));

  assert (matches (And (With a, With b)) (Id.ComponentSet.of_list [ b; a ]));
  assert (matches (And (With a, With b)) (Id.ComponentSet.of_list [ a; d; b ]));
  assert (
    not (matches (And (With b, With a)) (Id.ComponentSet.of_list [ b; d ])));
  assert (not (matches (And (With b, With d)) (Id.ComponentSet.of_list [ a ])));

  assert (matches (Or (With a, With b)) (Id.ComponentSet.of_list [ a; b ]));
  assert (matches (Or (With a, With b)) (Id.ComponentSet.of_list [ b; c; a ]));
  assert (matches (Or (With a, With b)) (Id.ComponentSet.of_list [ a; c ]));
  assert (not (matches (Or (With a, With b)) (Id.ComponentSet.of_list [ c ])));
  assert (not (matches (Or (With a, With b)) (Id.ComponentSet.of_list [])));

  assert (matches Wildcard (Id.ComponentSet.of_list [ a; b; c; d ]));
  assert (matches Wildcard (Id.ComponentSet.of_list []))

let test_required_components () =
  let open Query in
  let q = create [] in
  assert (required_components q = []);
  let q = create [ Required Foo.C.id ] in
  assert (required_components q = [ Foo.C.id ]);

  let q = create [ Required Foo.C.id; Optional Bar.C.id ] in
  assert (required_components q = [ Foo.C.id ]);

  let q = create [ Optional Bar.C.id; Optional Baz.C.id ] in
  assert (required_components q = []);

  let q = create [ Required Foo.C.id; Optional Baz.C.id; Required Bar.C.id ] in
  assert (required_components q = [ Foo.C.id; Bar.C.id ])

let test_evaluate () =
  (* TODO: Implement test_evaluate *)
  assert true

let () =
  test_filter_matches ();
  test_required_components ();
  test_evaluate ()
