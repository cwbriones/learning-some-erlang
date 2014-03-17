%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% A Balanced Binary Tree implementation using Red-Black trees.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(tree).
-export([
    new/0, 
    destroy/1, 
    insert/3,
    to_list/1,
    from_list/1,
    read/2
    %% delete/2, 
    %% match/2
  ]).

%% For operating on nodes
-type treedata() :: {term(), term()}.
-type color() :: red | black. 
-record(treenode, {data=undefined :: treedata(),
                   left=nil :: tree(), 
                   right=nil :: tree(), 
                   color=red :: color()}).
-type tree() :: #treenode{} | nil.

-spec new() -> tree().
new() -> nil.

-spec destroy(tree()) -> ok.
destroy(_) -> ok.

-spec insert(atom(), term(), tree()) -> tree().
insert(Key, Value, Tree) ->
  %% Wrapper to call the recursive function and ensure the root is black
  NewTree = ins(Key, Value, Tree),
  case NewTree of
    #treenode{color=red} -> NewTree#treenode{color=black};
    _ -> NewTree
  end.

-spec ins(atom(), term(), tree()) -> tree().
ins(Key, Value, nil) -> #treenode{color=red, data={Key, Value}, left=nil, right=nil};
ins(Key, Value, Tree = #treenode{data={Key, Value}}) -> Tree;
ins(Key, Value, Tree = #treenode{data={Key, _OldValue}}) ->
  Tree#treenode{data={Key, Value}};
ins(Key, Value, Tree = #treenode{data={CurKey, _CurValue}, left=Left, right=Right}) ->
  case Key < CurKey of
    true  -> balance(Tree#treenode{left=ins(Key, Value, Left)});
    false -> balance(Tree#treenode{right=ins(Key, Value, Right)})
  end.

-spec read(atom(), tree()) -> term() | {error, notfound}.
read(_, nil) -> {error, notfound};
read(Key, #treenode{data={Key, Value}}) -> Value;
read(Key, #treenode{data={CurKey, _CurValue}, left=Left, right=Right}) ->
  case Key < CurKey of
    true  -> read(Key, Left);
    false -> read(Key, Right)
  end.
    
-spec to_list(tree()) -> [{atom(), term()}].
to_list(nil) ->
  [];
to_list(#treenode{data=Data, left=L, right=R}) ->
  LeftList = to_list(L),
  RightList = to_list(R),
  lists:concat([LeftList, [Data | RightList]]).

-spec from_list([treedata()]) -> tree().
from_list(TreeData) ->
  from_list(TreeData, tree:new()).
-spec from_list([treedata()], tree()) -> tree().
from_list([], Tree) -> Tree;
from_list([{Key, Value} | Rest], Tree) ->
  from_list(Rest, tree:insert(Key, Value, Tree)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% Balances a tree assuming it was balanced previously and
%% a single item has broken the balance upon insertion.
%%
%% This is messy primarily because the pattern matching was
%% necessary to avoid incredibly nested case statements.
%%
%% Note: "[Child] is doubled [Grandchild]" means both child and grandchild are red.
%% 
-spec blacken(#treenode{}) -> #treenode{}.
blacken(Tree) ->
  Tree#treenode{color=black}.

-spec balance(tree()) -> tree().
balance(Tree = #treenode{color=red}) ->
  Tree;
balance(Tree = #treenode{
  left=Left=#treenode{color=red, left=LeftLeft=#treenode{color=red}}}) ->
  %% Left is doubled Left:
  NewLeft = LeftLeft#treenode{color=black},
  NewRight = Tree#treenode{left=Left#treenode.right},
  #treenode{color=red, data=Left#treenode.data, left=NewLeft, right=NewRight};
balance(Tree = #treenode{
  left=Left=#treenode{color=red, right=LeftRight=#treenode{color=red}}}) ->
  %% Left is doubled Right:
  NewData = LeftRight#treenode.data,
  NewLeft = Left#treenode{color=black,
                    right=(Left#treenode.right)#treenode.left},
  NewRight = Tree#treenode{left=Left#treenode.right#treenode.right},
  #treenode{color=red, data=NewData, left=NewLeft, right=NewRight};
balance(Tree = #treenode{
  right=Right=#treenode{color=red, left=#treenode{color=red}}}) ->
  %% Right is doubled Left:
  NewData = Right#treenode.left#treenode.data,
  NewLeft = Tree#treenode{right=Right#treenode.left#treenode.left},
  NewRight = Right#treenode{color=black,
                     left=Right#treenode.left#treenode.right},
  #treenode{color=red, data=NewData, left=NewLeft, right=NewRight};
balance(Tree = #treenode{
  right=Right=#treenode{color=red, right=#treenode{color=red}}}) ->
  %% Right is doubled Right:
  NewLeft = Tree#treenode{right=Right#treenode.left},
  NewRight = (Right#treenode.right)#treenode{color=black},
  #treenode{color=red, data=Right#treenode.data, left=NewLeft, right=NewRight};
balance(BalancedTree) ->
  BalancedTree.
