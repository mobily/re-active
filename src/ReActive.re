[@bs.module] external isEqual : ('a, 'b) => bool = "react-fast-compare";

module type Impl = {
  type t;
  type primaryKey;
  let primaryKey: t => primaryKey;
  let name: string;
};

module type Intf = {
  module rec Model: {
    type t;
    type primaryKey;
    type observable = {
      .
      next: t => unit,
      raw: t,
      stream: Callbag.stream(t),
    };
    let primaryKey: t => primaryKey;
    let make: t => observable;
    let update: (t => t, observable) => unit;
    let destroy: observable => unit;
    let save: observable => unit;

    module Observer: {
      type action =
        | OnNext(t);
      type state = t;
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
  }
  and Collection: {
    type t = list((Model.primaryKey, Model.observable));
    type observer = {
      raw: t,
      model: option(Model.t),
    };
    type observable = {
      .
      next: (t, option(Model.t)) => unit,
      notify: option(Model.t) => unit,
      raw: t,
      stream: Callbag.stream(observer),
    };
    let list: observable;
    let add: Model.observable => unit;
    let remove: Model.observable => unit;
    let clear: unit => unit;

    module Observer: {
      type action =
        | OnNext(observer);
      type state = observer;
      let make:
        (t => ReasonReact.reactElement) =>
        ReasonReact.componentSpec(
          state,
          state,
          ReasonReact.noRetainedProps,
          ReasonReact.noRetainedProps,
          action,
        );
    };
  };
};

module Make =
       (M: Impl)
       : (
           Intf with type Model.t = M.t and type Model.primaryKey = M.primaryKey
         ) => {
  module rec Model: {
    type t = M.t;
    type primaryKey = M.primaryKey;
    type observable = {
      .
      next: t => unit,
      raw: t,
      stream: Callbag.stream(t),
    };
    let primaryKey: t => primaryKey;
    let make: t => observable;
    let update: (t => t, observable) => unit;
    let destroy: observable => unit;
    let save: observable => unit;

    module Observer: {
      type action =
        | OnNext(t);
      type state = t;
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
  } = {
    type t = M.t;
    type primaryKey = M.primaryKey;
    class observable (value: t) = {
      as self;
      val mutable raw = value;
      val subject = Callbag.BehaviorSubject.make(value);
      pub raw = raw;
      pri subject = subject;
      pub stream = Callbag.(subject |> BehaviorSubject.asStream);
      pub next = value => {
        raw = value;
        Callbag.(self#subject |. BehaviorSubject.next(value));
        Collection.(Some(self#raw) |. list#notify);
      };
    };
    let primaryKey = M.primaryKey;
    let make = new observable;
    let update = (fn, observable) => fn(observable#raw) |> observable#next;
    let destroy = observable => Collection.remove(observable);
    let save = observable => Collection.add(observable);

    module Observer = {
      type action =
        | OnNext(t);
      type state = t;
      let component =
        ReasonReact.reducerComponent(M.name ++ "ReActiveModelObserver");
      let make = (~observable, children) => {
        ...component,
        initialState: () => observable#raw,
        shouldUpdate: ({oldSelf, newSelf}) => ! isEqual(oldSelf, newSelf),
        reducer: (action, _state) =>
          switch (action) {
          | OnNext(raw) => ReasonReact.Update(raw)
          },
        subscriptions: self => [
          Sub(
            () =>
              Callbag.(
                observable#stream
                |. subscribe(
                     ~next=raw => self.send(OnNext(raw)),
                     ~complete=Js.log,
                     ~error=Js.log,
                   )
              ),
            Callbag.unsubscribe,
          ),
        ],
        render: self => children(self.state),
      };
    };
  }
  and Collection: {
    type t = list((Model.primaryKey, Model.observable));
    type observer = {
      raw: t,
      model: option(Model.t),
    };
    type observable = {
      .
      next: (t, option(Model.t)) => unit,
      notify: option(Model.t) => unit,
      raw: t,
      stream: Callbag.stream(observer),
    };
    let list: observable;
    let add: Model.observable => unit;
    let remove: Model.observable => unit;
    let clear: unit => unit;

    module Observer: {
      type action =
        | OnNext(observer);
      type state = observer;
      let make:
        (t => ReasonReact.reactElement) =>
        ReasonReact.componentSpec(
          state,
          state,
          ReasonReact.noRetainedProps,
          ReasonReact.noRetainedProps,
          action,
        );
    };
  } = {
    type t = list((Model.primaryKey, Model.observable));
    type observer = {
      raw: t,
      model: option(Model.t),
    };
    class observable (value: t) = {
      as self;
      val mutable raw = value;
      val subject = Callbag.Subject.make();
      pub raw = raw;
      pri subject = subject;
      pub stream =
        Callbag.(
          subject
          |. Subject.asStream
          |. flatMap(model => just({raw: self#raw, model}))
        );
      pub notify = model => Callbag.(self#subject |. Subject.next(model));
      pub next = (value, model: option(Model.t)) => {
        raw = value;
        self#notify(model);
      };
    };
    let list = (new observable)([]);
    let add = model => {
      let list' =
        Belt.List.setAssoc(
          list#raw,
          Model.primaryKey(model#raw),
          model,
          (===),
        );
      list#next(list', Some(model#raw));
    };
    let remove = model => {
      let list' =
        Belt.List.removeAssoc(list#raw, M.primaryKey(model#raw), (===));
      list#next(list', Some(model#raw));
    };
    let clear = () => list#next([], None);

    module Observer = {
      type action =
        | OnNext(observer);
      type state = observer;
      let component =
        ReasonReact.reducerComponent(M.name ++ "ReActiveCollectionObserver");
      let make = children => {
        ...component,
        initialState: () => {model: None, raw: list#raw},
        shouldUpdate: ({oldSelf, newSelf}) => ! isEqual(oldSelf, newSelf),
        reducer: (action, _state) =>
          switch (action) {
          | OnNext(observer) => ReasonReact.Update(observer)
          },
        subscriptions: self => [
          Sub(
            () =>
              Callbag.(
                list#stream
                |. subscribe(
                     ~next=observer => self.send(OnNext(observer)),
                     ~complete=Js.log,
                     ~error=Js.log,
                   )
              ),
            Callbag.unsubscribe,
          ),
        ],
        render: self => children(self.state.raw),
      };
    };
  };
};
