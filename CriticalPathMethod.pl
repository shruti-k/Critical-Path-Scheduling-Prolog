%finding the maximum value and storing in M
max(A,B,M):-
	M is max(A,B).
	
%calls this function recursively to calculate
%early finish of parent tasks
earlyFinish(T, Time):-
	findall(ParentTask, prerequisite(T,ParentTask),Parents),
	getParentTime(Parents,0,ParentTime),
	duration(T, ChildTime),
	Time is ChildTime + ParentTime.

getParentTime([],Max,Max).

getParentTime([H|T],MaxTime,Max):-
	earlyFinish(H,Time),
	max(Time, MaxTime, NewMax),
	getParentTime(T,NewMax,Max).

%finding the minimum value and storing in M
min(A,B,M):-
	M is min(A,B).

%finds the last node in the graph
findLast(X):-
	findall(X,duration(X,D),Y1),
	findall(X,prerequisite(D,X),Y2),
	subtract(Y1,Y2,[X|_]).
	
	
%calls this function recursively to calculate
%late start of child tasks
lateStart(T, Time):-
	findall(ChildTask, prerequisite(ChildTask,T),Children),
	findLast(X),
	earlyFinish(X,MAXT),
	getChildTime(Children,MAXT,ChildTime),
	duration(T, ParentDuration),
	Time is ChildTime - ParentDuration.

getChildTime([],Min,Min).

getChildTime([H|T],MinTime, Min):-
	lateStart(H,Time),
	min(Time, MinTime, NewMin),
	getChildTime(T,NewMin,Min).


calcMax(Val,Node,CurrMax,CurrNode,NewMax,NewNode):-
  Val > CurrMax, !,
  NewMax = Val,
  NewNode = Node;
  NewMax = CurrMax,
  NewNode = CurrNode.

%finds the node with max slack value by comparing
%with node with prev max value in graph
calculateMaxSlack([],MaxV,MaxN,MaxV,MaxN).
calculateMaxSlack([H|T],CurrMax,CurrNode,MaxV,MaxN):-
	earlyFinish(H,EF),
	lateStart(H,LS),
	duration(H,D),
	Slack is LS + D - EF,
	calcMax(Slack,H,CurrMax,CurrNode,NewMax,NewNode),
	calculateMaxSlack(T,NewMax,NewNode,MaxV,MaxN).

%finds the root node in the graph	
findRoot(X):-
	findall(X,duration(X,D),Y1),
	findall(X,prerequisite(X,D),Y2),
	subtract(Y1,Y2,[X|_]).

%calculates the node with the max slack
maxSlack(Task,S):-
	findall(X,duration(X,D),T),
	findRoot(R),
	subtract(T,[R],C),
	earlyFinish(R,EF),
	lateStart(R,LS),
	duration(R,D),
	Slack is LS + D - EF,
	calculateMaxSlack(C,Slack,R,MaxV,MaxN),
	Task = MaxN,
	S = MaxV.

findMax(Val,Node,CurrMax,CurrNode,NewMax,NewNode):-
  Val > CurrMax, !,
  NewMax = Val,
  NewNode = Node;
  NewMax = CurrMax,
  NewNode = CurrNode.

%compares current value with current max value
%and updates the current max if current value is greater
calcMaxParent([],MaxV,MaxN,MaxV,MaxN).
calcMaxParent([H|T],CurrMax,CurrNode,MaxV,MaxN):-
	earlyFinish(H,EF),
	findMax(EF,H,CurrMax,CurrNode,NewMax,NewNode),
	calcMaxParent(T,NewMax,NewNode,MaxV,MaxN).
	
%appending root node to the critical path
calcCriticalPath(T,Path,PathF):-
	findRoot(T),
	append([T],Path,PathF).

%finds the parent with max early finish value
%by  calling itself recursively
calcCriticalPath(T,Path,PathF):-
	findall(ParentTask, prerequisite(T,ParentTask),P),
	calcMaxParent(P,0,T,MaxVal,MaxNode),
	append([T],Path,PathNew),
	calcCriticalPath(MaxNode,PathNew,PathF).
	
criticalPath(T,P):-
	calcCriticalPath(T,[],P).	