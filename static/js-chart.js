

var plotChart = function(container_committed, container_expected, committed, expected) {

    var width = 240, height = 500;
/*
    var chart = d3.select(".chart")
        .attr("width", width)
        .attr("height", height);

*/

    var barWidth = width / (committed.length + expected.length);

    var y = d3.scale.linear()
        .domain([0, d3.max(expected)])
        .range([height, 0]);

    var bar_committed = d3.select(container_committed).selectAll("g")
        .data(committed)
        .enter().append("g")
        .attr("transform", function(d, i) { return "translate(" + i * barWidth + ",0)"; });

    bar_committed.append("rect")
        .attr("class", "committed")
        .attr("y", function(d) { return y(d); })
        .attr("height", function(d) { return height - y(d); })
        .attr("width", barWidth - 1)

    bar_committed
        .append("text")
        .attr("y", function(d) { return y(d) - 8; })
        .attr("x", barWidth / 2)
        .attr("dy", ".35em")
        .text(function(d) { return "$" + d; });

    var bar_expected = d3.select(container_expected).selectAll("g")
        .data(expected)
        .enter().append("g")
        .attr("transform", function(d, i) { return "translate(" + (i + committed.length) * barWidth + ",0)"; });

    bar_expected.append("rect")
        .attr("class", "expected")
        .attr("y", function(d) { return y(d); })
        .attr("height", function(d) { return height - y(d); })
        .attr("width", barWidth - 1);

}
