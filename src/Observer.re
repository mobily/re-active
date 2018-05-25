module type Observer = {
  type t;
  type observable;
  let observer: observable => Callbag.stream(t);
  let initialState: observable => t;
};

module type Intf = {
  type t;
  type observable;
  type action =
    | OnNext(t);
  type state = t;
  let component:
    ReasonReact.componentSpec(
      state,
      ReasonReact.stateless,
      ReasonReact.noRetainedProps,
      ReasonReact.noRetainedProps,
      action,
    );
  let make:
    (~observable: observable, t => ReasonReact.reactElement) =>
    ReasonReact.componentSpec(
      state,
      state,
      ReasonReact.noRetainedProps,
      ReasonReact.noRetainedProps,
      action,
    );
};

module Make =
       (M: Observer)
       : (Intf with type t := M.t and type observable := M.observable) => {
  type action =
    | OnNext(M.t);
  type state = M.t;
  let component = ReasonReact.reducerComponent("ReactiveObserver");
  let make = (~observable, children) => {
    ...component,
    initialState: () => M.initialState(observable),
    reducer: (action, _state) =>
      switch (action) {
      | OnNext(raw) => ReasonReact.Update(raw)
      },
    subscriptions: self => [
      Sub(
        () =>
          M.observer(observable)
          |> Callbag.subscribe(
               ~next=raw => self.send(OnNext(raw)),
               ~complete=Js.log,
               ~error=Js.log,
             ),
        Callbag.unsubscribe,
      ),
    ],
    render: self => children(self.state),
  };
};
