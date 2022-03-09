% Follow paper exactly.  https://arxiv.org/abs/1804.02452 See Appendix A
% Difference from unison model.mzn: Try not using integers. Try to use better minizinc syntax. Much simplified

include "globals.mzn";

%% ---------------------------------------
%% PARAMETERS
%% ---------------------------------------

% Type definitions
% All are enumerative types
enum reg_t;
enum opcode_t;
enum temp_t;
enum hvar_t;
enum operand_t;
enum operation_t;
enum block_t;



array[operand_t] of operation_t : operand_operation; % map from operand to operations it belongs to
array[temp_t] of operand_t : definer; %map from temporary to operand that defines it
array[temp_t] of set of operand_t : users; %map from temporary to operands that possibly use it
array[temp_t] of block_t : temp_block; % map from temporary to block it lives in

set of operation_t : copy; % is operation a copy operation

array[temp_t] of int : width; % Atom width of temporary. Atoms are subpieces of registers. Maybe 8 bit.

array[operand_t] of set of reg_t : preassign; % preassign operand_t to register.

array[temp_t] of set of temp_t : congruent; % Whether congruent. Congruent operands are actually the same temporary split by linear SSA.

array[operation_t] of set of opcode_t : operation_opcodes; % map from operation to possible instructions that implement it

% register class
% For simd registers, spilling.

array[operand_t, opcode_t] of set of reg_t : class_t;

array[opcode_t] of int: latency; % latency of instruction


array[block_t] of operation_t : block_ins;
array[block_t] of operation_t : block_outs;
array[block_t] of set of operation_t : block_operations;

% map from user friendly names to temporaries derived them via linear ssa.
array[hvar_t] of set of temp_t : hvars_temps;

int: MAXC = 100; % maximum cycle. This should possibily be a parameter read from file.

% todo: resource parameter
% con, dur, cap

% todo: objective function parameters.
% weight
% cost

%% -------------------
%% Solution exclusion
%% ---------------------
int : number_excluded;
set of int : exclusions = 1..number_excluded;
array[exclusions, temp_t] of reg_t : exclude_reg;
/* array[exclusions, operation_t] of opcode_t : exclude_insn;
array[exclusions, operand_t] of temp_t : exclude_temp;
array[exclusions, temp_t] of bool : exclude_live;
array[exclusions, operation_t] of bool : exclude_active;
array[exclusions, operation_t] of 0..MAXC : exclude_issue; % maybe make these ranges. I could see a good argument for that
array[exclusions, temp_t] of 0..MAXC : exclude_start_cycle;
array[exclusions, temp_t] of 0..MAXC : exclude_end_cycle;
*/

%% ---------------------------------------------------------------
%%  VARIABLES
%% ---------------------------------------------------------------
array[temp_t] of var reg_t : reg;
array[operation_t] of var opcode_t : opcode;
array[operand_t] of var temp_t : temp;
array[temp_t] of var bool : live;
array[operation_t] of var bool : active;
array[operation_t] of var 0..MAXC : issue; % maybe make these ranges. I could see a good argument for that
array[temp_t] of var 0..MAXC : start_cycle;
array[temp_t] of var 0..MAXC : end_cycle;




%% ------------------------------
%% CONSTRAINTS
%% ------------------------------


% Exclude all previous solutions
constraint forall(n in exclusions)(
        exists( t in temp_t )( reg[t] != exclude_reg[n,t] )
    %\/  exists( o in operation_t )( opcode[o] != exclude_opcode[n,o] )
    %\/  exists( o in operand_t )( temp[o] != exclude_temp[n,o] )
    %\/  exists( t in temp_t )( live[t] != exclude_live[n,t] )
    %\/  exists( o in operation_t )( active[o] != exclude_active[n,o] )
    %\/  exists( o in operation_t )( issue[o] != exclude_issue[n,o] )
    %\/  exists( t in temp_t )( start_cycle[t] != exclude_start_cycle[n,t] )
    %\/  exists( t in temp_t )( end_cycle[t] != exclude_end_cycle[n,t] )
);


% function nonempty?


% constraint to choose instruction from allowed instructions. Why was this not implied
% not in the paper
% maybe if I had more logic interconnecting choice of temporary / register with opcode this would be implied
constraint forall(o in operation_t where card(operation_opcodes[o]) > 0 )(
    opcode[o] in operation_opcodes[o]
 );


% operand should only be assigned to temporary it either defines or uses.
% Is this implicit from other constraints?
constraint forall(o in operand_t)(
        temp[o] in { t | t in temp_t where definer[t] = o \/ o in users[t] }
);



/*
constraint forall(t in temp_t)(
    temp[definer[t]] = t
);

constraint forall(t in temp_t)(
    forall(o in operand_t)(
        exists() temp[o] = t
    )
    temp[users[t]] = t
);
*/

%C1.1
% no overlap constraint for live register usage
% use cumulative?
% use minizinc built-in diffn.
% The original unison model uses many different options
function array[int] of var int : block_array(array[temp_t] of var int : a, block_t : b) =
    [a[t] | t in temp_t where temp_block[t] = b ];

% [diffn_nonstrict]  https://www.minizinc.org/doc-latest/en/lib-globals.html#packing-constraints
% Constrains rectangles i , given by their origins ( x [ i ], y [ i ]) and
% sizes ( dx [ i ], dy [ i ]), to be non-overlapping. Zero-width rectangles can be packed anywhere.

% still need to include live[t] condition
constraint forall(blk in block_t)(
        diffn(block_array(reg,blk),  % built in global minizinc constraint
              block_array(start_cycle,blk),
            %  block_array([width[t] * bool2int(live[t]) | t in temp_t ], b), % no this is bad
            block_array(width, blk),
              [end_cycle[t] - start_cycle[t] | t in temp_t where temp_block[t] = blk])
);


% C2.1 Pre-assignment to registers
constraint forall(p in operand_t) (
                forall(r in preassign[p])(
                      reg[temp[p]] = r
                )
            );

% C3.2. register class constraint
constraint forall(p in operand_t where active[operand_operation[p]])(
    reg[temp[p]] in class_t[p, opcode[operand_operation[p]]]

);

% C4 Every operation that is not a copy must be active.
constraint forall(o in operation_t where not (o in copy))(
    active[o]
);

% C5.1  A temporary is live if its defining operation is active
constraint forall(t in temp_t)(
    live[t] = active[operand_operation[definer[t]]]
);

% C5.2 For an active operation there must be at least one live temporary available
constraint forall(t in temp_t)(
    %card({p | p in users[t] where live[t] == active[operand_operation[p]] /\ temp[p] == t}) >= 1 % fishy encoding
    live[t] <- exists(p in users[t])( active[operand_operation[p]] /\ temp[p] = t ) % should be both way implication?
);

% C6 Congruent temps map to the same register
constraint forall (p in temp_t, q in temp_t where q in congruent[p])(
    reg[p] = reg[q]
);

%% Instruction Scheduling Constraints

% C7.1  The issue cycle of operations that define temporaries must be before all
%       active operations that use that temporary
constraint forall (t in temp_t)(
     let {operand_t : p = definer[t]} in
        forall(q in users[t] where active[operand_operation[q]] /\ temp[q] == t)(
            issue[operand_operation[q]] >= issue[operand_operation[p]] + latency[opcode[operand_operation[p]]]
    )
);

% resource consumptions contraint
% TODO. We are not tracking resource consumption yet
/*
use cumulative

constraint forall (b in block_ts)(
    forall (s in Resource)
)
*/

%% Integration Constraints

% C9 The start cycle of a temporary is the issue cycle of it's defining operation
constraint forall (t in temp_t where live[t])(
   start_cycle[t] == issue[operand_operation[definer[t]]]
);

% Not in paper. Do we want this?
% end is at least past the start cycle plus the latency of the operation
/*
constraint forall (t in temp_t)(
    end[t] >= start[t] + latency[ opcode[ operand_operation[definer[t]] ]]
);
*/



%C10 Then end cycle of a temporary is the last issue cycle of operations that use it.
constraint forall (t in temp_t where card(users[t]) > 0 /\ live[t])(

   end_cycle[t] == max(
      % [ start[t] + 10 ] ++  % a kludge to make minizinc not upset when users[t] is empty.
       [ issue[operand_operation[p]]  | p in users[t] where temp[p] == t  ]  )
       % shouldn't this also be contingent on whether the user is active?
       % I suppose the temporary won't be live then.
);

% In blocks must be at start of block
% Outs must be at end

constraint forall (blk in block_t)(
    issue[block_ins[blk]] == 0
    /\
    % worse but possibly faster
    % issue[block_outs[blk]] == MAXC
    forall(o in block_operations[blk]) (issue[o] <= issue[block_outs[blk]])
);


% anything that has a where clause that isn't just parameters makes me queasy. What is this going to do?

%----------------------
% HELPERS
%----------------------

predicate set_reg(hvar_t: hvar, reg_t: r) =
 forall(t in hvars_temps[hvar])(reg[t] = r);

%--------------------------------
% OBJECTIVE
%-------------------------------

% Minimize number of registers
solve minimize card( {r | p in operand_t, r in reg_t where reg[temp[p]] = r } );


%TODO. For the moment, mere satisfaction would make me happy
% minimize max( [end[t] | t in temp_t ] ) % total estimated time (spilling is slow)
% minimize resource usage
% solve minimize max( end );
% solve minimize max( reg ); % use smallest number of registers
