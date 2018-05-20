include ReasonReact;

module Fragment = {
  [@bs.module "react"] external fragment : ReasonReact.reactClass = "Fragment";
  let make = children =>
    ReasonReact.wrapJsForReason(
      ~reactClass=fragment,
      ~props=Js.Obj.empty(),
      children,
    );
};
