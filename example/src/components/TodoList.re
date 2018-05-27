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
                 Js.log2("all", todos);
                 <div>
                   <div>
                     Belt.Array.(
                       map(todos, ((id, todo)) => <TodoItem key=id todo />)
                       |> array
                     )
                   </div>
                   <div>
                     (
                       string(
                         "completed: "
                         ++ Belt.Array.(
                              keep(todos, ((_id, todo)) => todo#raw.completed)
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
        /* only starred todos */
        <Collection.Observer
          observer=(
            stream =>
              Callbag.(
                stream
                |. map(({models, _}) =>
                     Collection.{
                       notifier: None,
                       models:
                         Belt.Array.keep(models, ((_id, todo)) => todo#raw.starred),
                     }
                   )
              )
          )>
          ...(
               todos => {
                 Js.log2("starred", todos);
                 <div>
                   Belt.Array.(
                     map(todos, ((id, todo)) => <TodoItem key=id todo />)
                     |> array
                   )
                 </div>;
               }
             )
        </Collection.Observer>
      </div>
    ),
};
