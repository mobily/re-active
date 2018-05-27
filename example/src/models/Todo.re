module Impl = {
  type t = {
    id: string,
    name: string,
    completed: bool,
  };
  type primaryKey = string;
  let primaryKey = (model: t) => model.id;
};

module Active = ReActive.Make(Impl);

module Model = {
  include Active.Model;
  let toggleCompleted = todo =>
    todo |> update(model => {...model, completed: ! model.completed});
};

module Collection = Active.Collection;

module Observer = {
  module Model = ReActive.Observer.Make(Model);
};
