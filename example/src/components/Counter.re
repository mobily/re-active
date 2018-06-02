open ReasonReact;

module Style = {
  open CssExtra;
  let root = style([display @: block]);
};

type action =
  | ChangeNumber(int);

type state = {number: int};

let component = reducerComponent("Counter");

let make = _children => {
  ...component,
  initialState: () => {number: 0},
  reducer: (action, _state) =>
    switch (action) {
    | ChangeNumber(number) => Update({number: number})
    },
  didMount: self => {
    let dispose =
      Wonka.(
        Todo.Collection.stream
        |> map(({models, _}: Todo.Collection.observer) =>
             Belt.Array.length(models)
           )
        /* |> distinctUntilChanged */
        |> subscribe(number => self.send(ChangeNumber(number)))
      );

    self.onUnmount(dispose);
  },
  render: self =>
    <div className=Style.root>
      (string(self.state.number |. string_of_int))
    </div>,
};
