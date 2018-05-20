module type Impl = {
  type t;
  type primaryKey;
  let default: t;
  let primaryKey: t => primaryKey;
};

module type Intf = {
  type t;
  type observable = Private.observable(t);
  type primaryKey;
  let make: t => observable;
  let update: (t => t, observable) => unit;
  let primaryKey: t => primaryKey;
  let default: t;
};

module Make =
       (M: Impl)
       : (Intf with type t = M.t and type primaryKey = M.primaryKey) => {
  type t = M.t;
  type primaryKey = M.primaryKey;
  class observable = class Private.observable(M.t);
  let make = new observable;
  let default = M.default;
  let update = (fn, observable) => fn(observable#raw) |> observable#next;
  let primaryKey = M.primaryKey;
};
