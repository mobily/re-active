type sink('a) = Wonka_types.signalT('a) => unit;

type stream('a) = sink('a) => unit;
