open ReActive__Types;

[@bs.module] external isEqual : ('a, 'b) => bool = "react-fast-compare";

module type Impl = {
  type t;
  type primaryKey;
  let name: string;
  let default: unit => t;
  let primaryKey: t => primaryKey;
};

module type Intf = {
  module rec Model: {
    type raw;
    type primaryKey;
    type t = {
      .
      next: raw => unit,
      raw: raw,
      stream: stream(raw),
    };
    let default: unit => raw;
    let primaryKey: raw => primaryKey;
    let make: (raw => raw) => t;
    let update: (t, raw => raw) => unit;
    let destroy: t => unit;
    let save: t => unit;

    module Observer: {
      type action =
        | OnNext(raw);
      type state = raw;
      let make:
        (~observable: t, raw => ReasonReact.reactElement) =>
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
    module CollectionComparator: {type t = Model.t; type identity;};
    type set =
      Belt.Set.t(CollectionComparator.t, CollectionComparator.identity);
    type models = array(CollectionComparator.t);
    type notifier =
      | Update(Model.raw)
      | Touch;
    type observer = {
      models,
      notifier,
    };
    type t = {
      .
      next: (~notifier: notifier=?, set) => unit,
      notify: notifier => unit,
      set: set,
      stream: stream(observer),
    };
    let instance: t;
    let stream: stream(observer);
    let add: Model.t => unit;
    let addMany: models => unit;
    let remove: Model.t => unit;
    let removeMany: models => unit;
    let clear: unit => unit;
    let updateMany: (models, Model.raw => Model.raw) => unit;

    module Observer: {
      type action =
        | OnNext(observer);
      type state = observer;
      let make:
        (
          ~observer: stream(observer) => stream(observer)=?,
          models => ReasonReact.reactElement
        ) =>
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
           Intf with
             type Model.raw = M.t and type Model.primaryKey = M.primaryKey
         ) => {
  module rec Model: {
    type raw = M.t;
    type primaryKey = M.primaryKey;
    type t = {
      .
      next: raw => unit,
      raw: raw,
      stream: stream(raw),
    };
    let default: unit => raw;
    let primaryKey: raw => primaryKey;
    let make: (raw => raw) => t;
    let update: (t, raw => raw) => unit;
    let destroy: t => unit;
    let save: t => unit;

    module Observer: {
      type action =
        | OnNext(raw);
      type state = raw;
      let make:
        (~observable: t, raw => ReasonReact.reactElement) =>
        ReasonReact.componentSpec(
          state,
          state,
          ReasonReact.noRetainedProps,
          ReasonReact.noRetainedProps,
          action,
        );
    };
  } = {
    type raw = M.t;
    type primaryKey = M.primaryKey;
    class t (value: raw) = {
      as self;
      val mutable raw = value;
      val subject = Wonka.makeSubject();
      pub raw = raw;
      pri subject = subject;
      pub stream = subject.observer;
      pub next = value => {
        raw = value;
        self#subject.next(value);
        Collection.(Update(self#raw) |. instance#notify);
      };
    };
    let default = M.default;
    let primaryKey = M.primaryKey;
    let make = fn => (new t)(fn(default()));
    let update = (observable, fn) => fn(observable#raw) |. observable#next;
    let destroy = observable => Collection.remove(observable);
    let save = observable => Collection.add(observable);

    module Observer = {
      type action =
        | OnNext(raw);
      type state = raw;
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
        didMount: self => {
          let dispose =
            Wonka.(
              observable#stream |> subscribe(raw => self.send(OnNext(raw)))
            );
          self.onUnmount(dispose);
        },
        render: self => children(self.state),
      };
    };
  }
  and Collection: {
    module CollectionComparator: {type t = Model.t; type identity;};
    type set =
      Belt.Set.t(CollectionComparator.t, CollectionComparator.identity);
    type models = array(CollectionComparator.t);
    type notifier =
      | Update(Model.raw)
      | Touch;
    type observer = {
      models,
      notifier,
    };
    type t = {
      .
      next: (~notifier: notifier=?, set) => unit,
      notify: notifier => unit,
      set: set,
      stream: stream(observer),
    };
    let instance: t;
    let stream: stream(observer);
    let add: Model.t => unit;
    let addMany: models => unit;
    let remove: Model.t => unit;
    let removeMany: models => unit;
    let clear: unit => unit;
    let updateMany: (models, Model.raw => Model.raw) => unit;

    module Observer: {
      type action =
        | OnNext(observer);
      type state = observer;
      let make:
        (
          ~observer: stream(observer) => stream(observer)=?,
          models => ReasonReact.reactElement
        ) =>
        ReasonReact.componentSpec(
          state,
          state,
          ReasonReact.noRetainedProps,
          ReasonReact.noRetainedProps,
          action,
        );
    };
  } = {
    module CollectionComparator =
      Belt.Id.MakeComparable({
        type t = Model.t;
        let cmp = compare;
      });
    type set =
      Belt.Set.t(CollectionComparator.t, CollectionComparator.identity);
    type models = array(CollectionComparator.t);
    type notifier =
      | Update(Model.raw)
      | Touch;
    type observer = {
      models,
      notifier,
    };
    class t (models: set) = {
      as self;
      val mutable set = models;
      val subject = Wonka.makeSubject();
      pub set = set;
      pri subject = subject;
      pub stream =
        Wonka.(
          subject.observer
          |> map(notifier => {notifier, models: Belt.Set.toArray(self#set)})
        );
      pub notify = notifier => self#subject.next(notifier);
      pub next = (~notifier=Touch, models) => {
        set = models;
        self#notify(notifier);
      };
    };
    let set = () => Belt.Set.make(~id=(module CollectionComparator));
    let instance = (new t)(set());
    let stream = instance#stream;
    let add = model => {
      let set = Belt.Set.add(instance#set, model);
      instance#next(set);
    };
    let remove = model => {
      let set = Belt.Set.remove(instance#set, model);
      instance#next(set);
    };
    let addMany = models => {
      let set = Belt.Set.mergeMany(instance#set, models);
      instance#next(set);
    };
    let removeMany = models => {
      let set = Belt.Set.removeMany(instance#set, models);
      instance#next(set);
    };
    let updateMany = (models, fn) =>
      Belt.Array.forEach(models, model => Model.(model |. update(fn)));
    let clear = () => instance#next(set());

    module Observer = {
      type action =
        | OnNext(observer);
      type state = observer;
      let component =
        ReasonReact.reducerComponent(M.name ++ "ReActiveCollectionObserver");
      let make = (~observer=stream => stream, children) => {
        ...component,
        initialState: () => {notifier: Touch, models: [||]},
        shouldUpdate: ({oldSelf, newSelf}) => ! isEqual(oldSelf, newSelf),
        reducer: (action, _state) =>
          switch (action) {
          | OnNext(observer) => ReasonReact.Update(observer)
          },
        didMount: self => {
          let dispose =
            Wonka.(
              observer(stream)
              |> subscribe(observer => self.send(OnNext(observer)))
            );
          self.onUnmount(dispose);
          instance#notify(Touch);
        },
        render: self => children(self.state.models),
      };
    };
  };
};
