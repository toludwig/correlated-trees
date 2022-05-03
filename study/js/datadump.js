
////////////////////
// constants
////////////////////
  
var url_IDs = window.location.search.split("?")[1];
const base_fee = 1.5;
const max_bonus = 3;

// save how often the participant failed the comprehension checks
var compcheck1_failed = 0;
var compcheck2_failed = 0;

// list of all walks
var walk_data = [];

// dumps data from walk
function dump_walk(city_no, city, walk_no, walk_reward, path, path_reward, path_timing, tooltip_nodes){
  var walk_row = {
    "city_no": city_no, "city": city,
    "walk_no": walk_no, "walk_reward": walk_reward,
    "path": path, "path_reward": path_reward,
    "path_timing": path_timing, "tooltip_nodes": tooltip_nodes
  };
  
  // append this walk to the list of all
  walk_data.push(walk_row);
  
  //console.log(JSON.stringify(walk_row));
  
  var file_name = '../data/walks_' + url_IDs + '.txt';
  $.post("results_data.php", {postresult: JSON.stringify(walk_row) + ",\n", postfile: file_name});
}

function dump_all_walks(){
  // at the end of the study, dump all walks again to be safe
  // (they could have lost internet connection during the experiment)
  if(url_IDs === undefined || url_IDs === "") {
    alert("Your Prolific ID is unknown, cannot save your data.");
    return;
  }
  var file_name = '../data/all_walks_' + url_IDs + '.txt';
  $.post("results_data.php", {postresult: JSON.stringify(walk_data) + "\n", postfile: file_name});
}

function dump_tmp_and_advance(key, value){
  // dump info accumulated on the way to carry it over to the final page
  // after successful dumping, advance website using a call to nextHTML()
  // NB: cannot just dump to data folder because we can't read from that and we need to read the bonus in the end
  if(url_IDs === undefined || url_IDs === "") {
    alert("Your Prolific ID is unknown, cannot save your data.");
    return;
  }
  
  var file_name = "../tmp/tmp_" + url_IDs + ".txt";
  $.post("results_data.php", {postresult: key + ": " + value + "\n", postfile: file_name},
         function(data){
            nextHTML(); // only after saving advance in HTML
            // TODO what if not successful?
         });
}

function dump_meta_and_show_bonus(){
  if(url_IDs === undefined || url_IDs === "") {
    alert("Your Prolific ID is unknown, cannot save your data.");
    return;
  }
  $.ajax({ // load the stored items from the tmp file
    "url": "../tmp/tmp_" + url_IDs + ".txt",
    "success": function(tmp_dict){
      tmp_dict = tmp_dict.split("\n");
      
      console.log(tmp_dict);
      
      // it's possible that there are multiple entries of failed compchecks
      // if people go back and forth in the instructions
      // therefore we need to add them up
      var compcheck1_failed = 0;
      var compcheck2_failed = 0;
      var total_score = 0;
      for(var i=0; i < tmp_dict.length; i++){
        var line = tmp_dict[i].split(": ");
        switch(line[0]){
          case "compcheck1_failed":
            compcheck1_failed += parseInt(line[1]); break;
          case "compcheck2_failed":
            compcheck2_failed += parseInt(line[1]); break;
          case "total_score":
            total_score = parseFloat(line[1]); break;
        }
      }
      
      var data = {
        "A": url_IDs.split("a=")[1].split("&")[0],
        "workerID": url_IDs.split("workerID=")[1].split("&")[0],
        //"assignmentID": url_IDs.split("assignmentID=")[1].split("&")[0],
        //"sessionID": url_IDs.split("sessionID=")[1],
        "age": age,
        "gender": gender,
        "colorblind": colorblind,
        "attention": attention,
        "fun": fun,
        "error_report": error_report,
        "compcheck1_failed": compcheck1_failed,
        "compcheck2_failed": compcheck2_failed,
        "total_score": total_score
      };
      
      var file_name = '../data/meta_' + url_IDs + '.txt';
      $.post("results_data.php", {postresult: JSON.stringify(data), postfile: file_name});
      // TODO only if saving was successful
      show_bonus(total_score);
    },
    "error": function(XMLHttpRequest, textStatus, errorThrown) { 
      alert("Oops, something went wrong with accessing your Prolific ID.");
    }
  });
}

////////////////////
// helpers
////////////////////

// copied from: https://stackoverflow.com/a/12646864
function shuffleArray(array) {
    for (var i = array.length - 1; i > 0; i--) {
        var j = Math.floor(Math.random() * (i + 1));
        var temp = array[i];
        array[i] = array[j];
        array[j] = temp;
    }
}


/////////////////////////////
// tree definitions
/////////////////////////////

const num_versions = 10; // for each tree there are multiple versions of reward priors

// create city list for instructions
function demo_city_list(a){
  return [
    {"H": 3, "B": 2, "A": a, "v": 0},
    {"H": 3, "B": 4, "A": a, "v": 0},
    {"H": 4, "B": 3, "A": a, "v": 1},
    {"H": 4, "B": 4, "A": a, "v": 1}
  ];
}

// create city list for tutorial
function tut_city_list(a){
  return [
    {"H": 3, "B": 3, "A": a, "v": 1},
    {"H": 2, "B": 2, "A": a, "v": 1},
    {"H": 4, "B": 4, "A": a, "v": 1}
  ];
}


// create city list for experiment
// TODO participant specific?
// TODO shuffle?
// TODO repetitions?
// TODO same version / mixed versions?
function exp_city_list(a){ // a = 0 for unstructrured, a = 1 for structured
  var list = [], i = 0;
  for(var h=2; h <= 4; h++)
    for(var b=2; b <= 4; b++){
      //for(var a=0.5; a <= 2; a *= 2){
      // pick a random version of this city
      var v = Math.floor(Math.random() * num_versions);
      list[i++] = {"H": h, "B": b, "A": a, "v": v};
    }
  
  // shuffle order of cities
  shuffleArray(list);
  return list;
}
