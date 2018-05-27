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
        <Collection.Observer>
          ...(
               todos => {
                 Js.log(todos);
                 <div>
                   <div>
                     Belt.List.(
                       map(todos, ((id, todo)) => <TodoItem key=id todo />)
                       |> toArray
                       |> array
                     )
                   </div>
                   <div>
                     (
                       string(
                         "completed: "
                         ++ Belt.List.(
                              keep(todos, ((_id, model)) =>
                                model#raw.completed
                              )
                              |> length
                              |> string_of_int
                            ),
                       )
                     )
                   </div>
                   <div onClick=(_e => Collection.clear())>
                     (string("clear all"))
                   </div>
                 </div>;
               }
             )
        </Collection.Observer>
      </div>
    ),
};
