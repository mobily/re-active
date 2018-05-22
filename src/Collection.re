open BsCallbag;

module type Intf = {
  type model;
  type primaryKey;
  type t = Private.observables(primaryKey, model);
  type observable = Private.observable(t);
  let initialState: observable => t;
  let observer: observable => Callbag.stream(t);
  let make: t => observable;
  let add: (model, observable) => unit;
  let remove: (model, observable) => unit;
  let clear: observable => unit;
};

module Make =
       (M: Model.Intf)
       : (
           Intf with
             type model := M.observable and type primaryKey := M.primaryKey
         ) => {
  type t = Private.observables(M.primaryKey, M.observable);
  class observable = class Private.observable(t);
  let make = new observable;
  let initialState = observable => observable#raw;
  let observer = observable => observable#stream;
  let add = (model, observable) => {
    let list =
      Belt.List.setAssoc(
        observable#raw,
        M.primaryKey(model#raw),
        model,
        (===),
      );
    observable#next(list);
  };
  let remove = (model, observable) => {
    let list =
      Belt.List.removeAssoc(observable#raw, M.primaryKey(model#raw), (===));
    observable#next(list);
  };
  let clear = observable => observable#next([]);
};
