#!/usr/bin/env node
// From https://github.com/zacharydenton/zach.se
var fs = require('fs');
var path = require('path');
var katex = require('katex');
var cheerio = require('cheerio');

var spawn = require('child_process').spawn,
      spawnSync = require('child_process').spawnSync,
      path = require('path'),
      fs = require('fs');
var highlight = require('pygments');
var input = '';


function executeSync(target, options) {
  var spawnRes = spawnSync(highlight.bin, highlight.convert_options(options), {input:target});
  if (spawnRes.stderr && spawnRes.stderr.toString().length > 1) {
    console.log("ERRORS: " + spawnRes.stderr.toString());
  }
  return spawnRes.stdout.toString();
}

function unwrap(el) {
	if (!el) {
		return false;
	}	else {
		var textnode = el.firstChild;
		var elParent = el.parentNode;
		elParent.replaceChild(textnode,el);
	}
}

function render(html, cb) {
  if (!html) {
    console.log("no html?");
    return;
  }
	var $ = cheerio.load(html);

	// Run KaTeX to generate math blocks
	$('.math').each(function() {
		var $el = $(this);
		if ($el.children().length == 0) {
			var content = $el.text();
			try {
				var math = katex.renderToString(content, {displayMode: $el.is('.display')});
			}
			catch (e) {
				console.log("broke trying to read: \"" + content + "\"");
				throw e;
			}
			$el.html(math);
		}
	});

	// Run pygments on code blocks
	var codes = $('pre > code');
	codes.each(function() {
		var $el = $(this);
		var code = $el.text();
		var pre = $el.parent();
		var lang = pre.attr('class');

    var colorizeParameters = highlight.prepareColorizeParameters(code, lang, 'html');
    var data = executeSync(colorizeParameters.target, colorizeParameters.options);
    if (!data || data.length < 1) {
      console.log("highlighting broke somehow");
    } else {
      pre.replaceWith(data);
    }
  });
  cb($.html());
}

if (process.argv.length > 2) {
	// list of files as arguments
	process.argv.slice(2).forEach(function(path) {
    console.log(path);
		fs.readFile(path, function(err, html) {
      render(html, function(processed) {
        fs.writeFile(path, processed, function(){});
      });
    });
	});
} else {
  console.log("pass file paths as args");
}
