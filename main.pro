% onur dilsiz
% 2019400036 
% compiling: yes
% complete: yes
:- ['cmpecraft.pro'].

:- init_from_map.
:- discontiguous minimum_of_list/3.

% 10 points
% this predicate calculates distance between 2 points 
manhattan_distance([],[],0).
manhattan_distance([H|T],[H1|T1],N):- D is H1-H, D1 is abs(D), manhattan_distance(T,T1,N1),N is N1 +D1.

% manhattan_distance(+A, +B, -Distance) :- .
% 10 points
% minimum_of_list(+List, -Minimum) :- .

% finds the minimum of a list  using min predicate of prolog
minimum_of_list([H|T], Min) :-
    minimum_of_list(T, H, Min).

minimum_of_list([], Min, Min).
minimum_of_list([H|T], Min0, Min) :-
    Min1 is min(H, Min0),
    minimum_of_list(T, Min1, Min).
% 10 points

%find_x_type(State,Typee,Bag):-State=[A,O,T],findall(Obj,(g et_dict(_,O,Obj),get_dict(type,Obj,Typee)),Bag).


% collects selected type of objects' keys into a list
find_x_type(State,Typee,Bag):-State=[A,O,T],findall(X,O.X.type=Typee,Bag).
%calculates distance between agent and object
calculate_distance(A,O,D):- get_dict(x,O,Ox),get_dict(y,O,Oy),manhattan_distance([A.x,A.y],[Ox,Oy],D).

% this predicate finds the closest ObjectType in the given State.
find_nearest_type(State, ObjectType, ObjKey, Object, Distance) :-
    State=[A,O,T],find_x_type(State,ObjectType,Bag),findall(D,(member(X,Bag), get_dict(X,O,Objectt),calculate_distance(A,Objectt,D)),Dbag),minimum_of_list(Dbag,Distance),member(ObjKey,Bag),get_dict(ObjKey,O,Object),calculate_distance(A,Object,Distance).
    
% 10 points
%these predicates gives a list which move in X amount in X direction, and Y amount in Y direction. 
navigate_tool(X, Y, L2,L2) :- X is 0,Y is 0,!.
navigate_tool(X, Y, L,L3) :- X < 0, X1 is X+1, append(L,[go_left],L2), navigate_tool(X1, Y,L2,L3).
navigate_tool(X, Y, L,L3) :- X > 0, X1 is X-1, append(L,[go_right],L2), navigate_tool(X1, Y,L2,L3).
navigate_tool(X, Y, L,L3) :- Y > 0, Y1 is Y-1, append(L,[go_down],L2), navigate_tool(X, Y1,L2,L3).
navigate_tool(X, Y, L,L3) :- Y < 0, Y1 is Y+1, append(L,[go_up],L2), navigate_tool(X, Y1,L2,L3).

%this predicate gives an ActionList that will navigate the agent to X and Y location.
navigate_to(State, X, Y, ActionList, DepthLimit) :- 
    State=[A,O,T],get_dict(x,A,Ax),get_dict(y,A,Ay),
    Xc is X-Ax, Yc is Y-Ay, Total is Xc+Yc, Total=< DepthLimit, 
    navigate_tool(Xc,Yc, [],ActionList).
    
%navigate_to(+State, +X, +Y, -ActionList, +DepthLimit)
% 10 points
% Appends N clicks into a list
append_Nclick(N, L,L) :-N is 0,!.
append_Nclick(N, L,L3) :- N > 0, X1 is N-1, append(L,[left_click_c],L2), append_Nclick(X1,L2,L3).

% generates the necessary actions to find the nearest tree, navigate to it and chop it
chop_nearest_tree(State,ActionList):- 
    State= [A,O,T], find_nearest_type(State,tree, ObjKeyy,Objectt,Dist), get_dict(x,Objectt,Ox), get_dict(y, Objectt,Oy),
    navigate_to(State, Ox, Oy, Actions ,Dist), append_Nclick(4,Actions,ActionList).
% chop_nearest_tree(+State, -ActionList) :- .
% 10 points
% generates the necessary actions to find the nearest stone, navigate to it and mine it
mine_nearest_stone(State, ActionList) :-
    State= [A,O,T], find_nearest_type(State,stone, ObjKeyy,Objectt,Dist), get_dict(x,Objectt,Ox), get_dict(y, Objectt,Oy),
    navigate_to(State, Ox, Oy, Actions ,Dist), append_Nclick(4,Actions,ActionList).
% generates the necessary actions to find the nearest cobblestone, navigate to it and mine it
mine_nearest_cobblestone(State, ActionList) :-
    State= [A,O,T], find_nearest_type(State,cobblestone, ObjKeyy,Objectt,Dist), get_dict(x,Objectt,Ox), get_dict(y, Objectt,Oy),
    navigate_to(State, Ox, Oy, Actions ,Dist), append_Nclick(4,Actions,ActionList).
% mine_nearest_stone(+State, -ActionList) :- .
% 10 points
% % generates the necessary actions to find the nearest food, navigate to it and gather it
gather_nearest_food(State, ActionList) :- 
    State= [A,O,T], find_nearest_type(State,food, ObjKeyy,Objectt,Dist), get_dict(x,Objectt,Ox), get_dict(y, Objectt,Oy),
    navigate_to(State, Ox, Oy, Actions ,Dist), append_Nclick(1,Actions,ActionList).
    
% gather_nearest_food(+State, -ActionList) :- .

%getters for log, cobblestone and stick from the inventory 
get_logC(Inv,LogCount):-findall(LogCount,get_dict(log, Inv, LogCount),Bag), length(Bag, 0),LogCount is 0,!.
get_logC(Inv,LogCount):-findall(LogCount,get_dict(log, Inv, LogCount),Bag), length(Bag, 1).
get_CobC(Inv,CobblestoneCount):-findall(CobblestoneCount,get_dict(cobblestone, Inv, CobblestoneCount),Bag), length(Bag, 0),CobblestoneCount is 0,!.
get_CobC(Inv,CobblestoneCount):-findall(CobblestoneCount,get_dict(cobblestone, Inv, CobblestoneCount),Bag), length(Bag, 1).

get_stickC(Inv,StickCount):-findall(StickCount,get_dict(log, Inv, StickCount),Bag), length(Bag, 0),StickCount is 0,!.
get_stickC(Inv,StickCount):-findall(StickCount,get_dict(log, Inv, StickCount),Bag), length(Bag, 1).

% collects according to needs logcount, cobblestonecount and stick count
collect_tool(State,State,Lc,Cc,Sc,L,L):-Lc>=0,Cc>=0,Sc>=0,!.
collect_tool(State,NewS2,Lc,Cc,Sc,L,L2):-
    State=[A,O,T],Sc<0, Lc>=2,Lcd is Lc -2,
    Scp is 4,append(L,[craft_stick],L1),collect_tool(State,NewS2,Lcd,Cc,Scp,L1,L2).
collect_tool(State,NewS2,Lc,Cc,Sc,L,L2):-
    State=[A,O,T],Sc<0, Lc<2, chop_nearest_tree(State,Actions),execute_actions(State,Actions,NewS),append(L,Actions,L5),append(L5,[craft_stick],L1),
    Lcd is Lc +1,Scp is 4,collect_tool(NewS,NewS2,Lcd,Cc,Scp,L1,L2).
collect_tool(State,NewS2,Lc,Cc,Sc,L,L2):-
    State=[A,O,T],Lc<0, chop_nearest_tree(State,Actions),append(L,Actions,L1),Lcp is Lc+3,execute_actions(State,Actions,NewS),collect_tool(NewS,NewS2,Lcp,Cc,Sc,L1,L2).
collect_tool(State,NewS2,Lc,Cc,Sc,L,L2):-
    State=[A,O,T],Cc<0, mine_nearest_stone(State,Actions),append(L,Actions,L1),CCp is Cc+3,
    execute_actions(State,Actions,NewS),collect_tool(NewS,NewS2,Lc,CCp,Sc,L1,L2).
collect_tool(State,NewS2,Lc,Cc,Sc,L,L2):-
    State=[A,O,T],Cc<0, mine_nearest_cobblestone(State,Actions),append(L,Actions,L1),CCp is Cc+1,
    execute_actions(State,Actions,NewS),collect_tool(NewS,NewS2,Lc,CCp,Sc,L1,L2).

% collects requirements for stick
collect_requirements(State, stick, []) :- State=[A,O,T], get_dict(inventory, A, Inv),get_logC(Inv,LogCount), LogCount>=2,!.
collect_requirements(State, stick, ActionList) :- State=[A,O,T], get_dict(inventory, A, Inv),get_logC(Inv,LogCount), LogCount<2, chop_nearest_tree(State,ActionList).

%collects requirements for stone_pickaxe
collect_requirements(State,stone_pickaxe , ActionList) :-
    State=[A,O,T], get_dict(inventory, A, Inv),get_logC(Inv,LogCount), Lc is LogCount -3,
    get_CobC(Inv,CobblestoneCount),Cc is CobblestoneCount-3 ,
    get_stickC(Inv,StickCount), Sc is StickCount-2,
    collect_tool(State,NS,Lc,Cc,Sc,[],ActionList).

%collects requirements for stone_axe
collect_requirements(State,stone_axe , ActionList) :-
    State=[A,O,T], get_dict(inventory, A, Inv),get_logC(Inv,LogCount), Lc is LogCount -3,
    get_CobC(Inv,CobblestoneCount),Cc is CobblestoneCount-3 ,
    get_stickC(Inv,StickCount), Sc is StickCount-2,
    collect_tool(State,NS,Lc,Cc,Sc,[],ActionList).

% 10 points
% collect_requirements(+State, +ItemType, -ActionList) :- .

% 5 points
% find_castle_location(+State, -XMin, -YMin, -XMax, -YMax) :- .
%get_X_Y(O,X,Y):-findall([X,Y],get_dict(log, Inv, LogCount),Bag), length(Bag, 0),LogCount is 0,!.
% gives true if there exists any object at point X,Y in the given state
tile_occupied1(X, Y, State) :-
    State = [_, StateDict, _],
    get_dict(_, StateDict, Object),
    get_dict(x, Object, Ox),
    get_dict(y, Object, Oy),
    X = Ox, Y = Oy.

% finds 3*3 free location
find_castle_location(State, XMin, YMin, XMax, YMax) :-
    State=[A,O,T], width(W),W1 is W-2,height(H),H1 is H-4,findall([X,Y],(maplist(between(1,W1 ),[X]),maplist(between(1,H1),[Y]),
    \+tile_occupied1(X,Y,State),X1 is X+1 ,\+tile_occupied1(X1,Y,State),X2 is X1+1,\+tile_occupied1(X2,Y,State),Y1 is Y+1,
    \+tile_occupied1(X,Y1,State),\+tile_occupied1(X1,Y1,State),\+tile_occupied1(X2,Y1,State), Y2 is Y1+1,
    \+tile_occupied1(X,Y2,State),\+tile_occupied1(X1,Y2,State),\+tile_occupied1(X2,Y2,State)    
    ),Bag),member([XMin,YMin],Bag),XMax is XMin +2, YMax is YMin+2 .

% 15 points
% make_castle(+State, -ActionList) :- .
% gives actions to makes castle by collecting requirements, finding suitable location and placing cobblestones
make_castle(State,ActionList) :-
    State=[A,O,T], get_dict(inventory, A, Inv),
    get_CobC(Inv,CobblestoneCount),Cc is CobblestoneCount-9, 
    collect_tool(State,NS,0,Cc,0,[],ActionList1),NS=[A1,O1,T1], get_dict(x,A,Ax),get_dict(y,A,Ay),
    find_castle_location(State, XMin, YMin, XMax, YMax), CX is XMin +1, CY is YMin+1, manhattan_distance([Ax,Ay],[CX,CY],Dist),
    navigate_to(NS, CX, CY, ActionList2, Dist),append(ActionList1,ActionList2,ActionList3),
    append(ActionList3,[place_c,place_n,place_e,place_w,place_s,place_ne,place_nw,place_se,place_sw],ActionList)    .
   

