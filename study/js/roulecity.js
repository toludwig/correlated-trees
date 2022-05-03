/////////////////////////////////
// global variables and constants
/////////////////////////////////

var mode;
/* 3 modi:
 "demo": shows the full tree to present the reward structure in the instructions
 "tut": 3 cities to try out
 "exp": the full experiment
 */
 
// study
var total_score = 0;
 
// city
var city_list; // stores the cities seen in the tutorial/experiment
var H, B, A, version; // height, branching factor, alpha (correlation length scale) and version of city
var city_no = 0, city_reward = 0; // count and cumsum of city
var city_max_reward, city_min_reward, city_median_reward; // reward of best/median/worst path in city (see path_stats.csv)
var city_max; // 3 for tutorial, 27 in experiment
 
// walk
var parent, path, path_reward, path_timing;
var walk_no = 0, walk_reward = 0; // count and cumulative reward of walk
const walk_max = 15; // number of walks per city
var tooltip_time, tooltip_nodes = []; // store which tooltips were used on each walk

// tree
var root;
const sd_noise = 2;
const color_reward = d3.scaleLinear().domain([0, 99]).range(['beige', 'red']);// TODO try multi-hue: d3.interpolateOrRd
var laterals = [];

// graphics
const width = 700, height = 700; // svg size of #city // TODO rename?
const level_height = 120; // radial distance between nodes
var block_clicks = false; // block clicking nodes after the end of each walk
const block_click_duration = 1000; // in milliseconds

// for the bars plot // TODO remove this?
//const barwidth = 200, barheight = 150; // svg size of #bars
var xscale, yscale;

/////////////////////////////
// experiment logic
/////////////////////////////

function next_city_params(){
  if(document.getElementById("manual") !== null){ // if manual config exists, take that TODO make less hacky
    // read tree parameters from manual config
    H = document.getElementById("H").value;
    B = document.getElementById("B").value;
    A = document.getElementById("A").value;
    version = document.getElementById("v").value;
  } else {
    H = city_list[city_no-1].H;
    B = city_list[city_no-1].B;
    A = city_list[city_no-1].A;
    version = city_list[city_no-1].v;
  }
}

function city_animation(){
  // fade out city
  d3.selectAll(".roulette, .tree")
    .attr("opacity", 1)
    .transition()
    .duration(2000)
    .attr("opacity", 0);
    
  // destroy city  
  d3.selectAll(".tree > *")
    .remove();
        
  // fade in city
  d3.selectAll(".roulette, .tree")
    .attr("opacity", 0)
    .transition()
    .duration(2000)
    .attr("opacity", 1);
    
  // rocket animation
  if(city_no == 1) // only land
    d3.select("#rocket")
      .transition()
      .duration(1000)
      .ease(d3.easeExpInOut)
      //.ease(d3.easeBackOut)
      .attr("y", height/2 - 30);
  else // take off and land again
    d3.select("#rocket")
      .transition()
      .duration(1000)
      .ease(d3.easeExpInOut)
      .attr("y", height/2 - 500)
      .transition()
      .duration(1000)
      .ease(d3.easeExpInOut)
      //.ease(d3.easeBackOut)
      .attr("y", height/2 - 30);
}

function init_city(svg, noiseflag){
  if(city_no === city_max){ // if all cities done
    if(mode === "tut")
      showQuiz(); // go to quiz
    else if(mode === "exp")
      dump_tmp_and_advance("total_score", total_score / 100 / city_max);
    
    return; // just to finish function
  }
  
  city_no++;
  d3.select("#city_count").text("City: " + city_no + "/" + city_max);
  
  walk_no = 0;
  d3.select("#walk_reward").text("");
  
  city_reward = 0;
  walk_reward = 0;
  
  // update H, B, A and version
  next_city_params();
  
  // load tree from csv
  var csv = "./trees/H" + H + "B" + B + "A" + A + "v" + version + ".csv";
  loadTree(H,B);
  function loadTree(H, B) { // closure for H and B to make them local variables
                            // important for instruction page where 2 trees with different parameters are drawn
    d3.csv(csv, function(data){
      // stratify makes the flat csv hierarchical
      root = d3.stratify()
                .id(function(d) { return d.name; })
                .parentId(function(d) { return d.parent; })
                (data);
      
      // put root in the center of the svg
      root.x = width / 2;
      root.y = height / 2;
      root.phi = Math.PI / 2;
    
      // assigns the x and y position for all nodes
      arrangeChildren(root, B);
      
      // collapse tree / sample all rewards (in demo)
      init_tree(root, noiseflag);
    
      // draw roulette
      roulette(svg, width, height, H, B, level_height);
      
      // start only if not in demo mode
      if(mode !== "demo"){
        city_animation();
        init_walk(svg);
        //init_bars();
      } else{ // if in demo:
        update_tree(svg);
      }
    });
  }
  
  
  // read max cumulative reward
  d3.csv("trees/path_stats.csv", function(data){
    var tmp = data.filter(function(row){
        return row.H === H.toString()
            && row.B === B.toString()
            && row.A === A.toString()
            && row.v === version.toString();
    });
    city_max_reward = tmp[0].best_reward;
    city_min_reward = tmp[0].worst_reward;
    city_median_reward = tmp[0].median;
    
    // TODO just for the demo
    if(mode == "demo")
      d3.select("#worst_path").text(""+ tmp[0].worst_reward);
      d3.select("#best_path").text(""+ tmp[0].best_reward);
      d3.select("#avg_path").text(""+ tmp[0].mean);
      d3.select("#sd_path").text(""+ tmp[0].sd);
  });
}



// recursive function to init each subtree
// initially a tree is collapsed
function init_tree(d, noiseflag) {
  d.rewardhistory = [];
  
  if(mode === "demo"){
    // in demo mode we have the choice to add noise or not
    if(noiseflag)
      sample_reward(d);
    else
      d.rewardlast = parseInt(d.data.reward);
    // in demo mode we want to see all values
    d.visited = true;
  } else{
    // in exp / tut mode we want to see only the visited values
    d.visited = false;
  }
  
  if(d.children) { // if not a leaf node
    d.children.forEach(function(c){init_tree(c, noiseflag);}); // recursion
  }
}

function init_walk(svg){
  // if 15 walks done, start new city
  if(walk_no === walk_max){
    show_feedback();
    return;
  }
  
  // only for tutorial!
  if(mode === "tut" && city_no === 1 && 0 < walk_no && walk_no < 4)
    nextSection();
  
  // update previous bar
  //if(walk_no !== 0)
  //  update_bars();
  
  walk_reward = 0;
  path_reward = [];
  
  path_timing = [];
  path_timing.push(Date.now()); // initial time of walk
  
  walk_no++;
  d3.select("#walk_count").text("Walk: " + walk_no + "/" + walk_max);
  
  parent = root;
  path = [];
  tooltip_nodes = [];
  
  update_tree(svg);
  
  // clear bars
  //d3.selectAll(".bars").remove();
}


// draw x ~ N(0,1) by Box-Mueller sampling
function gaussian_noise() {
  var u = 0, v = 0;
  while(u === 0) u = Math.random(); // use [0,1) instead (0,1)
  while(v === 0) v = Math.random();
  return Math.sqrt(-2.0 * Math.log(u)) * Math.cos(2.0 * Math.PI * v);
}


function sample_reward(node) {
  var noise = Math.round(gaussian_noise() * sd_noise);
  var tmp = parseInt(node.data.reward) + noise;
  if(tmp < 1) tmp = 1; // cap
  if(tmp > 99) tmp = 99; // cap
  node.rewardlast = tmp;
  node.rewardhistory.push(node.rewardlast);
  node.rewardmean = mean(node.rewardhistory);
}

function mean(x){
  var sum = 0;
  for(var i = 0; i < x.length; i++)
    sum += x[i];
  return sum / x.length; 
}


// All the transition logic happens in this function
function click_node(d) {
  if(mode === "demo") return; // do nothing
  
  if(block_clicks) return; // block additional clicks at the end of a walk
  
  if(!parent.children.includes(d)) {
    console.log("Error: you can only select direct child nodes.");
    return;
  }
  
  // if valid click, remember its timing
  path_timing.push(Date.now()); // milliseconds since page loaded
  
  // make child visited and receive reward
  d.visited = true;
  sample_reward(d);
  walk_reward += d.rewardlast;
  path_reward.push(d.rewardlast);
  path.push(d.id);
  
  var svg = d3.select("#city");
  
  if(d.children) { // if not at a leaf yet
    // advance level
    parent = d;
    update_tree(svg);
  } else { // if at a leaf node, walk done!
    block_clicks = true; // start blocking additional clicks
    
    d3.select("#walk_reward")
      .transition()
      .duration(1000)
      .text("+" + walk_reward)
      .transition()
      .duration(1000)
      .text("");
      
    city_reward += walk_reward;
    
    update_tree(svg);
    
    if(mode === "exp") // TODO don't save in tutorial?
      dump_walk(city_no, city_list[city_no-1], walk_no, walk_reward, path, path_reward, path_timing, tooltip_nodes);
      
    if(mode === "exp" && city_no === city_max && walk_no === walk_max){ // end of game
      dump_all_walks(); // dump a security copy of all walks,
      // in case some did not get saved during the game, e.g. due to bad internet connection
    }
      
    // start time in which clicks are blocked
    window.setTimeout(function(){
      block_clicks = false;
      init_walk(svg);
    }, block_click_duration); // TODO dump_all_walks depends on this being long enough!!!
  }
}


/////////////////////////
// D3 stuff, styles
/////////////////////////

function init_svg(svg){
  // Set the dimensions and margins of the svg
  var margin = {top: 90, right: 90, bottom: 90, left: 90};
  
  // the svg object to the body of the page
  svg.attr("width", width + margin.right + margin.left)
    .attr("height", height + margin.top + margin.bottom);
    
  // for drawing the rocket
  svg.attr("xmlns", "http://www.w3.org/2000/svg");
  svg.attr("xmlns:xlink", "http://www.w3.org/1999/xlink");
    
  // moves the 'group' elements to the top left margin
  svg.selectAll("g")
    .attr("transform", "translate(" + margin.left + "," + margin.top + ")");
  
  // the root gets overlayed with a square node
  svg.select(".counter").append("rect")
    .attr("x", width/2 - 40)
    .attr("y", height/2 - 40)
    .attr("width", "80px")
    .attr("height", "80px")
    .style("fill", "#eee");
    
  // draw rocket
  svg.select(".counter").append("image")
    .attr("id", "rocket")
    .attr("x", width/2 - 20)
    .attr("y", height/2 - 500)
    .attr("width", "40px")
    .attr("height", "40px")
    .attr("xlink:href", "img/rocket.png");
  
  svg.select(".counter").append("text")
    .attr("id", "walk_reward")
    .attr("x", width/2 - 25)
    .attr("y", height/2 + 30)
    .text("");
  
  svg.select(".counter").append("text")
    .attr("id", "walk_reward_text")
    .attr("x", 50)
    .attr("y", height - 20)
    .text("");
    
  // textfield progress
  if(mode !== "demo"){
    svg.select(".counter")
      .append("text")
      .text("City: 1/" + city_max)
      .attr("x", -45)
      .attr("y", -15)
      .attr("id", "city_count");
      
    svg.select(".counter")
      .append("text")
      .text("Walk: 1/" + walk_max)
      .attr("x", -45)
      .attr("y", 10)
      .attr("id", "walk_count");
  }
}

// draws the background
function roulette(svg, width, height, H, B, level_height){
  // before redrawing, remove old contents
  var g = svg.select("g.roulette")
  g.selectAll("path, circle").remove();
  
  var cx = width / 2, cy = height / 2;
  
  var i = 0; // color counter
  
  for(var lvl = parseInt(H)-1; lvl > 0; lvl--){
    // draw layer circles
    var radius = level_height * (lvl + 0.5);
    g.append("circle")
      .attr("r", radius)
      .attr("transform", "translate(" + width/2 + "," + height/2 + ")")
      .style("fill", "#fff")
      .style("stroke-width", "2px")
      .style("stroke", "#000");
      
    // draw segments on the circle for each degree
    var angle = 2*Math.PI / Math.pow(parseInt(B), lvl-1);
    for(var phi = -Math.PI / 2; phi < 1.5*Math.PI; phi += angle){
      g.append("path")
        .attr('d', `M ${cx},${cy}
                    L ${cx + radius * Math.cos(phi)},${cy + radius * Math.sin(phi)}
                    A ${radius},${radius} 1 0,1
                      ${cx + radius * Math.cos(phi+angle)},${cy + radius * Math.sin(phi+angle)} + z`
        )
        .style("fill", "#fff")
        .style("stroke-width", "2px")
        .style("stroke", "#000");
    }
  }
}


// custom tree layout function assigning coordinates to nodes
// only needs to be called once for the root, works recursively
function arrangeChildren(node, B) {
  var angle = 2*Math.PI / Math.pow(parseInt(B), node.depth+1); // on layer h there are B^h nodes
  var offset = node.phi - angle * (parseInt(B)-1)/2; // start clockwise from parent
  
  if(node.children) // if at leaf node, stop recursion
    node.children.forEach(function(d, i){
      d.phi = offset + angle * i;
      d.x = root.x + level_height * d.depth * Math.cos(d.phi);
      d.y = root.y + level_height * d.depth * Math.sin(d.phi);
      arrangeChildren(d, B); // recursion!
    });
}


// show feedback at the end of each round / city
function show_feedback() {
  d3.select(".modal").style("display", "block");
  
  var norm = city_max_reward - city_min_reward; // performance relative to what is achievable
  var percent_score = Math.round((city_reward/walk_max - city_min_reward) / norm * 100);
  if(norm == 0) // in the unlikely case that the worst and best path are equal (UPDATE: ruled out in generator)
    percent_score = 100;
  
  if(percent_score > 100) // cap at 100 (can be higher because of noise)
    percent_score = 100;
  if(percent_score < 0) // cap at 0 (can be lower because of noise)
    percent_score = 0;
    
  total_score += percent_score; // update sum of all scores
  
  document.getElementById("feedback").innerHTML =
    "In this city you collected " + Math.round(city_reward) + " units of energium in total.<br>"
  + "That is <b>" + percent_score + "%</b> of the best possible performance.<br>"
  + "Your spaceship is ready to launch again!";
}

function hide_feedback() {
  d3.select(".modal").style("display", "none");
  // when dialog is closed, go to next city
  init_city(d3.select("#city"), true);
}


function get_laterals(d){
  // make a list of lateral connections of currently expanded nodes
  if(!d.children) return; // recursion end
  for(var c=0; c < d.children.length-1; c++)
    if(parseInt(d.children[c].id) + 1 == parseInt(d.children[c+1].id)) // only if kids are consecutive
      laterals.push(
        {"source": d.children[c],
         "target": d.children[c+1]
        });
  d.children.forEach(get_laterals);
}

// main drawing function
function update_tree(svg) {
  var nodes = root.descendants(),
      links = root.links();
      
  // get laterals
  laterals = [];
  get_laterals(root);
      
  // ****************** Nodes section ***************************

  // Update the nodes...
  var node = svg.select(".tree").selectAll('g.node')
      .data(nodes, function(d) {return d.id;});
      
  var nodeEnter = node.enter().append('g')
    .attr('class', 'node')
    .attr("transform", function(d) {
      return "translate(" + d.x + "," + d.y + ")";
    });

  // Add Circle for the nodes
  nodeEnter.append('circle')
    .attr("r", function(d){
      if(mode === "demo") return 15;
      return parent.children.includes(d) ? 20 : 15; // current choices are bigger
    })
    .style("fill", function(d) {
      return d.visited ? color_reward(d.meanreward) : "#eee";
    })
    // mouse events
    .attr("cursor", function(d) {
      if(mode === "demo") return "default";
      return parent.children.includes(d) ? "pointer" : "default"; // current choices are clickable
    })
    .on('click', click_node)
    .on('mouseover', function(d) {
      tooltip_time = performance.now();
      // TODO custom tooltips? might be more controllable in timing...
      // var g = d3.select(this.parentNode);
      // g.append("rect")
      //   .attr("class", "tooltip")
      //   .style("background", "#f00")
      //   .attr("x", d.x + 10)
      //   .attr("y", d.y + 20);
      // 
      // g.append("text")
      //   .attr("class", "tooltip")
      //   .text(function(d){ return d.rewardhistory.toString(); });
    })
    .on('mouseout', function(d) {
      // if the mouse was on the node longer than 1s, count as seen tooltip
      if(performance.now() - tooltip_time > 1000) // TODO what is the correct timing of browser tooltips?
        tooltip_nodes.push(d.id);
        
      tooltip_time = undefined;
      //d3.select(".tooltip").remove();
    });
  
  nodeEnter.append('text')
    .attr("dy", ".35em")
    .attr("x", -5)
    .text(function(d) {
      return d.visited ? d.rewardlast.toString() : ""; // empty for non-visited
    })
    .style("pointer-events", "none"); // important so the tooltip timer does not get triggered by this
  
  nodeEnter.append("title")
    .text(function(d){ return d.rewardhistory.toString(); }); // set history as tooltip
  
  //---------------------------------
  var nodeUpdate = nodeEnter.merge(node);
  
  // Update the node attributes and style
  nodeUpdate.select('circle')
    .attr('r', function(d){
      if(mode === "demo") return 15;
      return parent.children.includes(d) ? 20 : 15; // current choices are bigger
    })
    .style("fill", function(d) {
      return d.visited ? color_reward(d.rewardlast) : "#eee";
    })
    .style("stroke", function(d) {
      if(mode === "demo") return "#fff";
      if(path.includes(d.id)) return "#00f";
      else if(parent.children.includes(d)) return "#00f";
      else return "#fff";
    })
    .attr("cursor", function(d) {
      if(mode === "demo") return "default";
      return parent.children.includes(d) ? "pointer" : "default"; // current choices are clickable
    });
  
  nodeUpdate.select('text')
    .attr("dy", ".35em")
    .attr("x", -7)
    .text(function(d) {
      return d.visited ? d.rewardlast.toString() : ""; // empty for non-visited
    })
    .attr("cursor", function(d) {
      if(mode === "demo") return "default";
      return parent.children.includes(d) ? "pointer" : "default"; // current choices are clickable
    });
    
  nodeUpdate.select("title")
    .text(function(d){ return d.rewardhistory.toString(); }); // set history as tooltip
  
  var nodeExit = node.exit().remove();
  
  // ****************** links section ***************************
  
  var link = svg.select(".tree").selectAll("path.link")
    .data(links, function(d) { return d.target.id; }); // target id is unique ID of link
  
  // Enter any new links at the source position.
  var linkEnter = link.enter().insert('path', "g")
    .attr("class", "link")
    .attr("d", function(d){ return street(d.source, d.target); });

  var linkUpdate = linkEnter.merge(link);
  linkUpdate
    //.attr('transform', 'translate(' + 
    .style("stroke-width", 2)
    .style("stroke", function(d){
      if(mode === "demo") return "#bbb";
      return path.includes(d.target.id) ? "#00f" : "#bbb";
    });

  // Remove any exiting links
  var linkExit = link.exit().remove();
    
    
  // ****************** lateral section ***************************
  
  var lateral = svg.select(".tree").selectAll("path.lateral")
    .data(laterals);
    
  lateral.exit().remove();
  
  var lateralEnter = lateral.enter().insert('path', "g")
    .attr("class", "link lateral")
    .attr('d', function(d){ return lateralStreet(d.source, d.target); })
    .style("stroke-width", 2)
    .style("stroke", "#bbb");
    
  lateralEnter.merge(lateral);

  // Creates a path from source to target
  function street(s, t) {
    return `M ${s.x},${s.y}
            L ${t.x},${t.y}`;
  }
  
  function lateralStreet(s, t) {
    var r = s.depth * level_height;
    return `M ${s.x},${s.y}
            A ${r},${r} 1 0,1 ${t.x} ${t.y}`;
  }
}


/* The following section would draw a bar plot with the walk_rewards over the 15 walks
 * but we decided to not show the history of previous walk_rewards to the participant
 * so as to not disencourage exploration (people might want to have a monotonic curve).

// inits the bar chart with the
function init_bars(){
  // before redrawing, remove old contents
  d3.select("#bars").select("g").remove();
  
  // Set the dimensions and margins of the svg
  var margin = {top: 50, right: 90, bottom: 30, left: 90};
  
  xscale = d3.scaleLinear().domain([0, walk_max]).range([0, barwidth]);
  yscale = d3.scaleLinear().domain([0, 100 * (H-1)]).range([barheight, 0]);
  
  // the svg object to the body of the page
  d3.select("#bars")
    .attr("width", barwidth + margin.right + margin.left)
    .attr("height", barheight + margin.top + margin.bottom);
    
  // moves the 'group' elements to the top left margin
  var g = d3.select("#bars").append("g")
    .attr("transform", "translate(" + margin.left + "," + margin.top + ")");
  
  // add the x axis
  g.append("g")
    .attr("transform", "translate(0," + barheight + ")")
    .call(d3.axisBottom(xscale));
    //.text("Walk no");

  // add the y axis
  g.append("g")
    .call(d3.axisLeft(yscale));
    //.text("Cum. reward");
}

function update_bars(){
  // append the rectangles for the bar chart
  d3.select("#bars g")
    .append("rect")
    .attr("class", "bar")
    .attr("x", function(d) { return xscale(walk_no); })
    .attr("width", xscale(1)*.5)
    .attr("y", function(d) { return yscale(walk_reward); })
    .attr("height", function(d) { return barheight - yscale(walk_reward); });
}

*/