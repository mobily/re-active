[@bs.module] external isEqual : ('a, 'b) => bool = "react-fast-compare";

module type Impl = {
  type t;
  type primaryKey;
  let name: string;
  let primaryKey: t => primaryKey;
  let default: unit => t;
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
    let default: unit => t;
    let make: (t => t) => observable;
    let update: (observable, t => t) => unit;
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
    module IdComparator: {type t = Model.observable; type identity;};
    type t = Belt.Set.t(IdComparator.t, IdComparator.identity);
    type observer = {
      raw: array(IdComparator.t),
      model: option(Model.t),
    };
    type observable = {
      .
      next: (t, option(Model.t)) => unit,
      notify: option(Model.t) => unit,
      raw: t,
      stream: Callbag.stream(observer),
    };
    let instance: observable;
    let stream: Callbag.stream(observer);
    let add: Model.observable => unit;
    let remove: Model.observable => unit;
    let clear: unit => unit;

    module Observer: {
      type action =
        | OnNext(observer);
      type state = observer;
      let make:
        (array(IdComparator.t) => ReasonReact.reactElement) =>
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
    let default: unit => t;
    let make: (t => t) => observable;
    let update: (observable, t => t) => unit;
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
      pub stream = Callbag.(subject |. BehaviorSubject.asStream);
      pub next = value => {
        raw = value;
        Callbag.(self#subject |. BehaviorSubject.next(value));
        Collection.(Some(self#raw) |. instance#notify);
      };
    };
    let primaryKey = M.primaryKey;
    let default = M.default;
    let make = fn => (new observable)(fn(default()));
    let update = (observable, fn) => fn(observable#raw) |. observable#next;
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
    module IdComparator: {type t = Model.observable; type identity;};
    type t = Belt.Set.t(IdComparator.t, IdComparator.identity);
    type observer = {
      raw: array(IdComparator.t),
      model: option(Model.t),
    };
    type observable = {
      .
      next: (t, option(Model.t)) => unit,
      notify: option(Model.t) => unit,
      raw: t,
      stream: Callbag.stream(observer),
    };
    let instance: observable;
    let stream: Callbag.stream(observer);
    let add: Model.observable => unit;
    let remove: Model.observable => unit;
    let clear: unit => unit;

    module Observer: {
      type action =
        | OnNext(observer);
      type state = observer;
      let make:
        (array(IdComparator.t) => ReasonReact.reactElement) =>
        ReasonReact.componentSpec(
          state,
          state,
          ReasonReact.noRetainedProps,
          ReasonReact.noRetainedProps,
          action,
        );
    };
  } = {
    module IdComparator =
      Belt.Id.MakeComparable({
        type t = Model.observable;
        let cmp = compare;
      });
    type t = Belt.Set.t(IdComparator.t, IdComparator.identity);
    type observer = {
      raw: array(IdComparator.t),
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
          |. flatMap(model => just({raw: Belt.Set.toArray(self#raw), model}))
        );
      pub notify = model => Callbag.(self#subject |. Subject.next(model));
      pub next = (value, model: option(Model.t)) => {
        raw = value;
        self#notify(model);
      };
    };
    let beltSet = () => Belt.Set.make(~id=(module IdComparator));
    let instance = (new observable)(beltSet());
    let stream = instance#stream;
    let add = model => {
      let set' = Belt.Set.add(instance#raw, model);
      instance#next(set', Some(model#raw));
    };
    let remove = model => {
      let set' = Belt.Set.remove(instance#raw, model);
      instance#next(set', Some(model#raw));
    };
    let clear = () => instance#next(beltSet(), None);

    module Observer = {
      type action =
        | OnNext(observer);
      type state = observer;
      let component =
        ReasonReact.reducerComponent(M.name ++ "ReActiveCollectionObserver");
      let make = children => {
        ...component,
        initialState: () => {
          model: None,
          raw: Belt.Set.toArray(instance#raw),
        },
        shouldUpdate: ({oldSelf, newSelf}) => ! isEqual(oldSelf, newSelf),
        reducer: (action, _state) =>
          switch (action) {
          | OnNext(observer) => ReasonReact.Update(observer)
          },
        subscriptions: self => [
          Sub(
            () =>
              Callbag.(
                stream
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
