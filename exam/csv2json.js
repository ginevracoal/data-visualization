var fs = require("fs")
var path = process.argv[2];
var text = fs.readFileSync(path).toString();
var lines = text.split('\n');
lines.shift();
var objs=lines.map((line,i)=>{
    var values = line.split(',');
    ii=String(i)
    var word = values[0]||ii;
    var link = values[1]||ii;
    var tf_idf=values[2]||ii;
    var tf=values[3]||ii;
    var owner=values[4]||ii;
    var n_weeks=values[5]||ii;
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
