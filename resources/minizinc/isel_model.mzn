%% Based on https://github.com/unison-code/uni-instr-sel/blob/master/solvers/minizinc/base-model.mzn
%% And thesis https://kth.diva-portal.org/smash/record.jsf?dswid=-5547
%%%%%%%%%%%%%%%%%%%%
%%% Input Parameters
%%%%%%%%%%%%%%%%%%%%
int: numMatches;

enum allOperationsInFunction;
%enum allOperands;

enum allDataInFunction;
% datum and operands are distinct concepts but can be equated for now.
set of allDataInFunction : allOperands = allDataInFunction;

array[allMatches] of set of allOperationsInFunction: operationsCoveredByMatch;
array[allMatches] of set of allOperands: operandsDefinedByMatch;
%array[allMatches] of set of int: operandsUsedByMatch;

%%%%%%%%%%%%%%%%%%%%%%
%%% Derived Parameters
%%%%%%%%%%%%%%%%%%%%%%

set of int: allMatches = 1..numMatches;

% The set of matches that can cover a particular operation.
array[allOperationsInFunction] of set of allMatches: matchsetOfOp =
  [ { m | m in allMatches where op in operationsCoveredByMatch[m] }
  | op in allOperationsInFunction
  ];

array[allOperands] of set of allDataInFunction: operandAlternatives =
  [{p} | p in allOperands];

% The set of matches that can define a particular datum.
array[allDataInFunction] of set of allMatches: defsetOfDatum =
  [ { m | m in allMatches
        , p in operandsDefinedByMatch[m]
          where d in operandAlternatives[p]
    }
  | d in allDataInFunction
  ];

% The set of matches that can define a particular datum.
%array[allOperands] of set of allMatches: defsetOfDatum =
%  [ { m | m in allMatches
%          where p in operandsDefinedByMatch[m]
%          % where d in operandAlternatives[p]
%    }
%  | p in allOperands
%  ];

%%%%%%%%%%%%%%%%%%%%
%%% VARIABLES
%%%%%%%%%%%%%%%%%%%

% Match selection.
array[allMatches] of var bool: sel;

% The match that covers a particular operation.
array[allOperationsInFunction] of var allMatches: omatch;

% The match that defines a particular datum.
array[allDataInFunction] of var allMatches: dmatch;

%%%%%%%%%%%%%%%%%
%%% CONSTRAINTS
%%%%%%%%%%%%%%%%

% For each operation o, exactly one match must be selected such that o is
% covered.
% Eq 5.1 in dissertation
constraint
  forall (o in allOperationsInFunction)
  ( omatch[o] in matchsetOfOp[o]
    /\
    forall (m in matchsetOfOp[o])
    ( omatch[o] = m <-> sel[m] )
  );

% For each datum d, exactly one match must be selected such that d is defined.
%
% Eq 5.2 in dissertation
constraint
  forall (d in allDataInFunction)
  ( dmatch[d] in defsetOfDatum[d]
    /\
    forall (m in defsetOfDatum[d])
    ( dmatch[d] = m <-> sel[m] )
  );

%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Objective function
%%%%%%%%%%%%%%%%%%%%%%%%%

%var int: totalcost = sum(m in allMatches)(bool2int(sel[m]) * cost[m]);
var int: totalcost = sum(m in allMatches)(bool2int(sel[m]));

solve minimize totalcost;

output [ show_int(10, omatch[o]) ++ " " | o in allOperationsInFunction ];