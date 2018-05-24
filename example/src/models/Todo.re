module Model = {
  module Impl = {
    type t = {
      id: string,
      name: string,
      completed: bool,
    };
    type primaryKey = string;
    let default = () => {
      id: SecureRandomString.genSync(),
      name: "",
      completed: false,
    };
    let primaryKey = (model: t) => model.id;
  };
  include Model.Make(Impl);
};

module Collection = Collection.Make(Model);

module Observer = {
  module Model = Observer.Make(Model);
  module Collection = Observer.Make(Collection);
};

let collection = Collection.make([]);
