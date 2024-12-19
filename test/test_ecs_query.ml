open Ecs
open Ecs.Query

let test_filter_matches () =
  let open Filter in
  assert (matches (With 1) (Id.ComponentSet.of_list [ 23; 1; 4 ]));
  assert (not (matches (With 1) (Id.ComponentSet.of_list [ 2 ])));
  assert (not (matches (With 1) (Id.ComponentSet.of_list [])));

  assert (matches (Without 42) (Id.ComponentSet.of_list []));
  assert (not (matches (Without 2) (Id.ComponentSet.of_list [ 3; 1; 2 ])));
  assert (matches (Without 2) (Id.ComponentSet.of_list [ 3; 1; 4 ]));

  assert (matches (Not (With 1)) (Id.ComponentSet.of_list [ 2 ]));
  assert (not (matches (Not (With 1)) (Id.ComponentSet.of_list [ 1 ])));

  assert (matches (And (With 1, With 2)) (Id.ComponentSet.of_list [ 1; 2 ]));
  assert (matches (And (With 1, With 2)) (Id.ComponentSet.of_list [ 2; 3; 1 ]));
  assert (
    not (matches (And (With 1, With 2)) (Id.ComponentSet.of_list [ 1; 3 ])));
  assert (not (matches (And (With 1, With 2)) (Id.ComponentSet.of_list [ 1 ])));

  assert (matches (Or (With 1, With 2)) (Id.ComponentSet.of_list [ 1; 2 ]));
  assert (matches (Or (With 1, With 2)) (Id.ComponentSet.of_list [ 2; 3; 1 ]));
  assert (matches (Or (With 1, With 2)) (Id.ComponentSet.of_list [ 1; 3 ]));
  assert (not (matches (Or (With 1, With 2)) (Id.ComponentSet.of_list [ 3 ])));
  assert (not (matches (Or (With 1, With 2)) (Id.ComponentSet.of_list [])));

  assert (matches Wildcard (Id.ComponentSet.of_list [ 1; 2; 3 ]));
  assert (matches Wildcard (Id.ComponentSet.of_list []))

let () = test_filter_matches ()
