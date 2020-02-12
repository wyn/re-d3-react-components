module D3 = {
  type selection;
  [@bs.module "d3"] external select: Dom.element => selection = "select";
  [@bs.send] external call: (selection, 'a) => unit = "call";
  [@bs.send]
  external transition: (selection, unit) => selection = "transition";
  [@bs.send] external attr: (selection, string, 'thing) => selection = "attr";

  module ScaleBand = {
    type t;
    [@bs.module "d3"] external create: 'a => t = "scaleBand";
    let get = (this: t, index: 'index) => {
      %raw
      {|$this($index)|};
    };
  };

  module ScaleLinear = {
    type t;
    [@bs.module "d3"] external create: ('a, 'b) => t = "scaleLinear";
    let get = (this: t, index: 'index) => {
      %raw
      {|$this($index)|};
    };
  };

  type scaleLinear;
  [@bs.send] external domain: ('scale, array('values)) => unit = "domain";
  [@bs.module "d3"] external axisBottom: ScaleBand.t => 'b = "axisBottom";
  [@bs.module "d3"] external axisLeft: ScaleLinear.t => 'a = "axisLeft";

  [@bs.send] external bandwidth: (ScaleBand.t, unit) => 'thing = "bandwidth";
};

type datum = {
  id: int,
  date: string,
  value: int,
};

type data = array(datum);

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
          ignore(D3.(select(element)->call(axisBottom(xScale))));
          None;
        | None =>
          ignore("");
          None;
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
          ignore(
            D3.(select(element)->transition()->call(axisBottom(xScale))),
          );
          None;
        | None =>
          ignore("");
          None;
        };
      },
      [|xScale|],
    );
    <g
      transform={j|`translate(0, $height)|j}
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
          ignore(D3.(select(element)->call(axisLeft(yScale))));
          None;
        | None =>
          ignore("");
          None;
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
          ignore(
            D3.(select(element)->transition()->call(axisLeft(yScale))),
          );
          None;
        | None =>
          ignore("");
          None;
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
        ~datum: datum,
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
            ),
          );
          None;
        | None =>
          ignore("");
          None;
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
            ),
          );
          None;
        | None =>
          ignore("");
          None;
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
        ~data: data,
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
  let make = (~svgHeight: int, ~svgWidth: int, ~data: data) => {
    let margin = {top: 20, right: 20, bottom: 30, left: 40};
    let width = svgWidth - margin.left - margin.right;
    let height = svgHeight - margin.top - margin.bottom;
    let xScaleJS: int => D3.ScaleBand.t = [%raw
      {| function (width) {
         return d3
         .scaleBand()
         .range([0, width])
         .padding(0.1)
         }
         |}
    ];
    let xScale = xScaleJS(width);
    let yScaleJS: int => D3.ScaleLinear.t = [%raw
      {| function (height) {
         return d3
         .scaleLinear()
         .range([height, 0])
         }
         |}
    ];
    let yScale = yScaleJS(height);
    Js.log(xScale);
    Js.log(yScale);
    xScale->D3.domain(data->Array.map(d => d.date));
    yScale->D3.domain([|0, 1|]);
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
  <Svg svgHeight=200 svgWidth=500 data=[|{id: 1, date: "boo", value: 1}|] />;
};
