try{
  if (!top.__nbcudigitaladops_inject){
	var __nbcudigitaladops_dtparams = "pm=1;";
	var __nbcudigitaladops_gptparams = "";
	var __nbcudigitaladops_inject = {
		init : function(){
			this.version = '2.8';

			var site = ''+location.hostname;
			site = site.replace(/^www\./,'').replace(/\./g,'_');
                 

			document.write('<scr'+'ipt src="//www.nbcudigitaladops.com/hosted/site.js?h='+site+'_header"></scr'+'ipt>');

			this.qsparam = 'dart';
			this.sitefilter = ['google.com','go','ask.com','as','aol.com','ao','yahoo.com','ya'];
			this.drt_r = 'ref';

			this.dtprm( this.qspass() );
			this.dtprm( this.ref() );
			this.dtprm( this.gc('adops_master_kvs') );
		},
		dtprm : function(p){
			top.__nbcudigitaladops_dtparams += p;
			this._setGPTGlobal();
			return top.__nbcudigitaladops_dtparams;
		},
		gc : function(n){
			var c=document.cookie;if(!c)return '';
			var i=c.indexOf(n+"=");if(-1==i)return '';
			var len=i+n.length+1;
			var end=c.indexOf(";", len);
			return unescape(c.substring(len,end<0?c.length:end));
		},
		qspass: function(){
			var qsval = this.getQSValue(this.qsparam);
			if(qsval!=''){
				qsval = this.qsparam+'='+qsval+';';
			}
			return qsval;
		},
		getQSValue: function(p){
			var qs=location.search+'';
			var i=qs.indexOf('&'+p+"="); // Find p after another param, avoid mid-param match
			if(-1==i){i=qs.indexOf('?'+p+"=");} // Find p as the first param
			if(-1==i){return '';}
			var len=i+p.length+2;
			var end=qs.indexOf("&", len);
			return qs.substring(len,end<0?qs.length:end);
		},
		ref: function(){
			var r = document.referrer,dlen = '',ref='',result='';
			r=r.replace(/^https*:\/\//i,'');
			r=r.split('/')[0];
			r=r.split('.');
			if(r!=''){
				ref=r[r.length-2]+'.'+r[r.length-1];
				dlen = r[r.length-2].length;
			}
			this.ref = ref;
			var f = this.sitefilter;
			for(var x=0;x<f.length;x+=2){
				if(f[x].toLowerCase() == this.ref){
					result = f[x+1]+dlen;
				}
			}
			if (result!=''){
				result = this.drt_r+'='+result+';';
			}
			return result;
		},
		getGPT: function(){
			var gpt = {};
			var dtparams = top.__nbcudigitaladops_dtparams.replace(/;+$/,'').split(/;/);
			for(var x=0;x<dtparams.length;x++){
				var d = dtparams[x].split(/=/);
				if( ! gpt.hasOwnProperty(d[0]) ){
					gpt[d[0]] = [];
				}
				gpt[d[0]].push(d[1]);
			}
			for ( var y in gpt ){
				if( gpt.hasOwnProperty(y) ){
					gpt[y] = gpt[y].join(',');
				}
			}
			return gpt;
		},
		_setGPTGlobal: function(){
			var gpt = this.getGPT();
			var gpt_arr = [];
			var gpt_var = '';
			for ( var y in gpt ){
				if( gpt.hasOwnProperty(y) ){
					gpt_arr.push(y+'%3D'+gpt[y]);
				}
			}
			gpt_var = gpt_arr.join('%26');
			__nbcudigitaladops_gptparams = gpt_var;
			_gpt_simpleUrlTargetingParams = __nbcudigitaladops_gptparams;
			return gpt_var;
		}

	};
	__nbcudigitaladops_inject.init();
  }
}catch(e){};

