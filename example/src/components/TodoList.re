open ReasonReact;

open Todo;

module Style = {
  open CssExtra;
  let root = style([display @: flexBox, flexDirection @: column]);
  let options =
    style([
      display @: flexBox,
      alignItems @: center,
      justifyContent @: spaceBetween,
      fontSize @: rem(1.8),
      marginBottom @: px(20),
      padding2(~v=zero, ~h=px(15)),
      userSelect @: none,
    ]);
};

let component = statelessComponent("TodoList");

let make = _children => {
  ...component,
  render: _self =>
    Style.(
      <div className=root>
        <Observer.Collection observable=collection>
          ...(
               todos =>
                 Belt.List.(
                   map(todos, ((id, todo)) => <TodoItem key=id todo />)
                   |> toArray
                   |> array
                 )
             )
        </Observer.Collection>
      </div>
    ),
};
