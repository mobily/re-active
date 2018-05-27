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
  subscriptions: self => [
    Sub(
      () =>
        Callbag.(
          Todo.Collection.stream
          |. map(({models, _}) => Belt.Array.length(models))
          |. distinctUntilChanged
          |. subscribe(
               ~next=number => self.send(ChangeNumber(number)),
               ~complete=Js.log,
               ~error=Js.log,
             )
        ),
      Callbag.unsubscribe,
    ),
  ],
  render: self =>
    <div className=Style.root>
      (string(self.state.number |. string_of_int))
    </div>,
};
