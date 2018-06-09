open ReasonReact;

open Todo;

module Style = {
  open CssExtra;
  let root = style([display @: flexBox, flexDirection @: row]);
  let half = style([flex @: 1]);
};

let component = statelessComponent("TodoList");

let make = _children => {
  ...component,
  render: _self =>
    Style.(
      <div className=root>
        <div className=half>
          <Collection.Observer>
            ...(
                 todos => {
                   Js.log2("all", todos);
                   <div>
                     <div>
                       (
                         string(
                           "completed: "
                           ++ Belt.Array.(
                                keep(todos, todo => todo#raw.completed)
                                |> length
                                |> string_of_int
                              ),
                         )
                       )
                     </div>
                     <div onClick=(_e => Collection.clear())>
                       (string("clear all"))
                     </div>
                     <div>
                       Belt.Array.(
                         map(todos, todo => <TodoItem key=todo#raw.id todo />)
                         |> array
                       )
                     </div>
                   </div>;
                 }
               )
          </Collection.Observer>
        </div>
        /* only starred todos */
        <div className=half>
          <Collection.Observer
            observer=(
              stream =>
                Wonka.(
                  stream
                  |> map(observer =>
                       Collection.{
                         ...observer,
                         models:
                           Belt.Array.keep(models, todo => todo#raw.starred),
                       }
                     )
                )
            )>
            ...(
                 todos => {
                   Js.log2("starred", todos);
                   <div>
                     <div
                       onClick=(
                         _e =>
                           todos
                           |. Belt.Array.keep(todo => todo#raw.completed)
                           |. Collection.removeMany
                       )>
                       (string("remove completed todos"))
                     </div>
                     <div
                       onClick=(
                         _e =>
                           todos
                           |. Collection.updateMany(model =>
                                {...model, completed: ! model.completed}
                              )
                       )>
                       (string("toggle completed"))
                     </div>
                     <div>
                       Belt.Array.(
                         map(todos, todo => <TodoItem key=todo#raw.id todo />)
                         |> array
                       )
                     </div>
                   </div>;
                 }
               )
          </Collection.Observer>
        </div>
      </div>
    ),
};
