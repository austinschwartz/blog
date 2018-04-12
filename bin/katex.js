#!/usr/bin/env node
// From https://github.com/zacharydenton/zach.se
var fs = require('fs');
var path = require('path');
var katex = require('katex');
var cheerio = require('cheerio');
var highlight = require('pygments').colorize;

var input = '';

var walkSync = function(dir, filelist) {
	files = fs.readdirSync(dir);
	filelist = filelist || [];
	files.forEach(function(file) {
		if (fs.statSync(path.join(dir, file)).isDirectory()) {
			filelist = walkSync(path.join(dir, file), filelist);
		}
		else {
			filelist.push(path.join(dir, file));
		}
	});
	return filelist;
};

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
	var processed = 0;
	var codes = $('pre > code');
	if (codes.length == 0)
		cb($.html());
	codes.each(function() {
		var $el = $(this);
		var code = $el.text();
		var pre = $el.parent();
		var lang = pre.attr('class');
		highlight(code, lang, 'html', function(data) {
			if (!data || data.length < 1) {
        console.log("code: " + code);
        console.log("lang: " + lang);
				throw "highlighting broke somehow";
			}
			pre.replaceWith(data);
			//pre.html(data);
			processed++;
			if (processed == codes.length)
				cb($.html());
		});
	});
}

if (process.argv.length > 2) {
	// list of files as arguments
	process.argv.slice(2).forEach(function(path) {
		var html = fs.readFileSync(path);
		render(html, function(processed) {
			fs.writeFileSync(path, processed);
		});
	});
} else {
	var files = walkSync('_site', []);
  files.forEach(function(path) {
    if (path.endsWith("html")) {
      var html = fs.readFileSync(path);
      render(html, function(processed) {
        fs.writeFileSync(path, processed);
      });
    }
  });
}
