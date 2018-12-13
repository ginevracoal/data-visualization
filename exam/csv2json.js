var fs = require("fs")
var path = process.argv[2];
var text = fs.readFileSync(path).toString();
var lines = text.split('\n');
lines.shift();
lines.pop();
var objs=lines.map((line,i)=>{
    var values = line.split(',');
    ii=String(i)
    var word = values[0]||ii;
    var link = values[1]||ii;
    var tf_idf=Number(values[2])||ii;
    var tf=Number(values[3])||ii;
    var owner=values[4]||ii;
    if(owner=="NA")
        owner='';
    var diff=Number(values[5])||ii;
    var n_weeks=Number(values[6])||ii;
    return {
        name: word,
        key: word,
        count: tf,
        tf,
        tf_idf,
        owner,
        n_weeks,
        pages: link.split('|').map((date)=>{
            return {
                key:date,
                name:date,
                title:date,
                url:date
            }
        })
    };
});
var content='window._my_data='+JSON.stringify(objs,"\t");
fs.writeFileSync(path+".js",content);
fs.writeFileSync("data.js",content);

// words table
var objs=lines.map((line,i)=>{
    var values = line.split(',');
    ii=String(i)
    var word = values[0]||ii;
    var link = values[1]||ii;
    var tf_idf=Number(values[2])||ii;
    var tf=Number(values[3])||ii;
    var owner=values[4]||ii;
    if(owner=="NA")
        owner='';
    var diff=Number(values[5])||ii;
    var n_weeks=Number(values[6])||ii;
    return {
        word,
        tf,
        tf_idf,
        owner,
        n_weeks,
        diff
    };
});

var content='window._my_words='+JSON.stringify(objs,"\t");
fs.writeFileSync(path+"_words.js",content);
fs.writeFileSync("words.js",content);