module D3 = {
  type selection;
  [@bs.module "d3"] external select: Dom.element => selection = "select";
  [@bs.send] external call: (selection, 'a) => unit = "call";
  [@bs.send]
  external transition: (selection, unit) => selection = "transition";
  [@bs.send] external attr: (selection, string, 'thing) => selection = "attr";

  module ScaleBand = {
    type t;
    [@bs.module "d3"] external create: unit => t = "scaleBand";
    [@bs.send] external range: (t, array(int)) => t = "range";
    [@bs.send] external padding: (t, float) => t = "padding";
    let get = (this: t, value: 'value): 'value =>
      [%raw {|
         function(scale, value) { return scale(value); }
    |}](
        this,
        value,
      );
  };

  module ScaleLinear = {
    type t;
    [@bs.module "d3"] external create: unit => t = "scaleLinear";
    [@bs.send] external range: (t, array(int)) => t = "range";
    let get = (this: t, value: 'value): 'value =>
      [%raw {|
         function(scale, value) { return scale(value); }
    |}](
        this,
        value,
      );
  };

  [@bs.send] external domain: ('scale, array('values)) => unit = "domain";
  [@bs.module "d3"] external axisBottom: ScaleBand.t => 'b = "axisBottom";
  [@bs.module "d3"] external axisLeft: ScaleLinear.t => 'a = "axisLeft";

  [@bs.send] external bandwidth: (ScaleBand.t, unit) => 'thing = "bandwidth";
};

type margin = {
  top: int,
  right: int,
  bottom: int,
  left: int,
};

module AxisBottom = {
  [@react.component]
  let make = (~height: int, ~xScale: D3.ScaleBand.t) => {
    let gRef = React.useRef(Js.Nullable.null);
    // first render
    React.useEffect1(
      () => {
        Js.log("AxisBottom first render");
        switch (gRef->React.Ref.current->Js.Nullable.toOption) {
        | Some(element) =>
          D3.(select(element)->call(axisBottom(xScale))); //D3.call(s, axis) == foreach thing in s: axis(thing)
          None;
        | None => None
        };
      },
      [||],
    );
    // update renders
    React.useEffect1(
      () => {
        Js.log("AxisBottom update render");
        switch (gRef->React.Ref.current->Js.Nullable.toOption) {
        | Some(element) =>
          D3.(select(element)->transition()->call(axisBottom(xScale)));
          None;
        | None => None
        };
      },
      [|xScale|],
    );
    // [%debugger];
    <g
      transform={j|translate(0, $height)|j}
      ref={ReactDOMRe.Ref.domRef(gRef)}
    />;
  };
};

module AxisLeft = {
  [@react.component]
  let make = (~yScale: D3.ScaleLinear.t) => {
    let gRef = React.useRef(Js.Nullable.null);
    // first render
    React.useEffect1(
      () => {
        Js.log("AxisLeft first render");
        switch (gRef->React.Ref.current->Js.Nullable.toOption) {
        | Some(element) =>
          D3.(select(element)->call(axisLeft(yScale)));
          None;
        | None => None
        };
      },
      [||],
    );
    // update renders
    React.useEffect1(
      () => {
        Js.log("AxisLeft update render");
        switch (gRef->React.Ref.current->Js.Nullable.toOption) {
        | Some(element) =>
          D3.(select(element)->transition()->call(axisLeft(yScale)));
          None;
        | None => None
        };
      },
      [|yScale|],
    );
    <g ref={ReactDOMRe.Ref.domRef(gRef)} />;
  };
};

module Bar = {
  [@react.component]
  let make =
      (
        ~datum: Data.datum,
        ~width: int,
        ~height: int,
        ~xScale: D3.ScaleBand.t,
        ~yScale: D3.ScaleLinear.t,
      ) => {
    let rectRef = React.useRef(Js.Nullable.null);
    // first render
    React.useEffect1(
      () => {
        Js.log("Bar first render");
        switch (rectRef->React.Ref.current->Js.Nullable.toOption) {
        | Some(element) =>
          // [%debugger];
          ignore(
            D3.(
              select(element)
              ->attr("fill", "green")
              ->attr("x", xScale->ScaleBand.get(datum.date))
              ->attr("y", yScale->ScaleLinear.get(datum.value))
              ->attr("height", 0)
              ->attr("width", xScale->bandwidth())
              ->transition()
              ->attr("height", height - yScale->ScaleLinear.get(datum.value))
            ) //
          );
          None;
        | None => None
        };
      },
      [||],
    );
    // update render
    React.useEffect2(
      () => {
        Js.log("AxisLeft update render");
        switch (rectRef->React.Ref.current->Js.Nullable.toOption) {
        | Some(element) =>
          ignore(
            D3.(
              select(element)
              ->attr("fill", "blue")
              ->transition()
              ->attr("x", xScale->ScaleBand.get(datum.date))
              ->attr("y", yScale->ScaleLinear.get(datum.value))
              ->attr("width", xScale->bandwidth())
              ->attr("height", height - yScale->ScaleLinear.get(datum.value))
            ) //
          );
          None;
        | None => None
        };
      },
      (xScale, yScale),
    );

    <rect ref={ReactDOMRe.Ref.domRef(rectRef)} />;
  };
};

module Bars = {
  [@react.component]
  let make =
      (
        ~data: Data.data,
        ~width: int,
        ~height: int,
        ~xScale: D3.ScaleBand.t,
        ~yScale: D3.ScaleLinear.t,
      ) => {
    let bars =
      data
      ->Array.map(datum => {
          <Bar
            key={datum.id->string_of_int}
            datum
            height
            width
            xScale
            yScale
          />
        })
      ->React.array;

    <g className="bars"> {height > 0 && width > 0 ? bars : React.null} </g>;
  };
};

module Svg = {
  [@react.component]
  let make = (~svgHeight: int, ~svgWidth: int, ~data: Data.data) => {
    let margin = {top: 20, right: 20, bottom: 30, left: 40};
    let width = svgWidth - margin.left - margin.right;
    let height = svgHeight - margin.top - margin.bottom;
    let xScale = D3.ScaleBand.(create()->range([|0, width|])->padding(0.1));
    let yScale = D3.ScaleLinear.(create()->range([|height, 0|]));
    //    [%debugger];
    Js.log(xScale);
    Js.log(yScale);
    xScale->D3.domain(data->Array.map(d => d.date));
    yScale->D3.domain([|
      0,
      data->Array.reduce(0, (acc, d) =>
        if (d.value > acc) {
          d.value;
        } else {
          acc;
        }
      ),
    |]);
    Js.log(xScale);
    Js.log(yScale);
    <svg height={svgHeight->string_of_int} width={svgWidth->string_of_int}>
      <g>
        <AxisBottom height xScale />
        <AxisLeft yScale />
        <Bars data height width xScale yScale />
      </g>
    </svg>;
  };
};

[@react.component]
let make = () => {
  let (data, setData) = React.useState(() => Data.getData());
  <div>
    <button onClick={_ => setData(_ => Data.getData())}>
      "DATA"->React.string
    </button>
    <Svg svgHeight=500 svgWidth=960 data />
  </div>;
};
