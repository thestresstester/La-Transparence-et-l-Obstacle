var CookieControlHelper=(function(){function CookieControlHelper(){}
function srcHandler(type,category){$(type+"[data-civicukcookie=\""+category+"\"]").each(function(){var dataSrc=$(this).attr("data-src");$(this).attr("src",dataSrc);$(this).removeAttr("data-civicukcookie");});}
CookieControlHelper.prototype.iframeHandler=function(category){srcHandler.call(this,'iframe',category);}
CookieControlHelper.prototype.imgHandler=function(category){srcHandler.call(this,'img',category);}
CookieControlHelper.prototype.imgHandler=function(category){}
CookieControlHelper.prototype.objectHandler=function(category){}
CookieControlHelper.prototype.linkHandler=function(category){$(type+"[data-civicukcookie=\""+category+"\"]").each(function(){var dataHref=$(this).attr("data-href");$(this).attr("href",dataHref);$(this).removeAttr("data-civicukcookie");});}
CookieControlHelper.prototype.scriptHandler=function(category){$("script[data-civicukcookie=\""+category+"\"]").each(function(){var scriptTag=document.createElement("script");scriptTag.innerHTML=$(this).html();$.each(this.attributes,function(){if(this.specified){scriptTag.setAttribute(this.name,this.value);}});scriptTag.type="text/javascript";scriptTag.removeAttribute("data-civicukcookie");$("body").append(scriptTag);});}
return CookieControlHelper;})();