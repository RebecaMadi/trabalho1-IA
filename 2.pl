block(a).
block(b).
block(c).
place(1).
place(2).
place(3).
place(4).

adds(move(X,From,To),[on(X,To),clear(From)]). 
deletes(move(X,From,To),[on(X,From),clear(To)]).

plan( State, Goals, []):-
  satisfied( State, Goals).                   % Goals true in State

plan( State, Goals, Plan):-
  append( PrePlan, [Action], Plan),           % Divide plan achieving breadth-first effect
  select( State, Goals, Goal),                % Select a goal
  achieves( Action, Goal),
  can( Action, Condition),                    % Ensure Action contains no variables
  preserves(Action,Goals),                    % Protect Goals
  regress( Goals, Action, RegressedGoals),    % Regress Goals through Action
  plan( State, RegressedGoals, PrePlan).

satisfied( State, Goals)  :-
  delete_all( Goals, State, []).             

select( State, Goals, Goal)  :-              
  member( Goal, Goals).                       

achieves( Action, Goal)  :-
  adds( Action, Goals),
  member( Goal, Goals).

preserves( Action, Goals)  :-                 
  deletes( Action, Relations),
  \+  (member( Goal, Relations),              % not member
       member( Goal, Goals) ).

regress( Goals, Action, RegressedGoals)  :-       % Regress Goals through Action
  adds( Action, NewRelations),
  delete_all( Goals, NewRelations, RestGoals),
  can( Action, Condition),
  addnew( Condition, RestGoals, RegressedGoals).

can(move(Block,From,To),[clear(Block),clear(To),on(Block,From)]):-
    block(Block),           % There is this Block to move
    object(To),             % 'T' is a block or place
    To \== Block,           % Block cannot be moved to itself
    object(From),           % 'From' is a block or place
    From \== To,            % 'To' is a new position to move
    Block \== From. 

addnew([], L, L).
addnew([Goal|_], Goals, _) :-
    impossible(Goal, Goals), !, fail.
addnew([X|L1], L2, L3) :-
    member(X, L2), !,
    addnew(L1, L2, L3).
addnew([X|L1], L2, [X|L3]) :-
    addnew(L1, L2, L3).

impossible(on(X, X), _).
impossible(on(X, Y), Goals) :-
    member(clear(Y), Goals);
    member(on(X, Y1), Goals), Y1 \== Y;
    member(on(X1, Y), Goals), X1 \== X.
impossible(clear(X), Goals) :-
    member(on(_, X), Goals).

delete_all([], _, []).
delete_all([X|L1], L2, Diff) :-
    member(X, L2),!,
    delete_all(L1, L2, Diff).
delete_all([X|L1], L2, [X|Diff]) :-
    delete_all(L1, L2, Diff).

object(X):-
    place(X);
    block(X).

member(X,[X|_]).
member(X,[_|T]):-
     member(X,T).