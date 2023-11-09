let run cfg_with_layout =
 (* note: the simplification of terminators is necessary for the equality. The
     other code path simplifies e.g. a switch with three branches into an
     integer test. This simplification should happen *after* the one about
     straightline blocks because merging blocks creates more opportunities for
     terminator simplification. *)
  Eliminate_fallthrough_blocks.run cfg_with_layout;
  Merge_straightline_blocks.run cfg_with_layout;
  Eliminate_dead_code.run_dead_block cfg_with_layout;
  Simplify_terminator.run (Cfg_with_layout.cfg cfg_with_layout)

