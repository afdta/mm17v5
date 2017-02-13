// https://d3js.org Version 4.2.6. Copyright 2016 Mike Bostock.
(function(t,n){"object"==typeof exports&&"undefined"!=typeof module?n(exports):"function"==typeof define&&define.amd?define(["exports"],n):n(t.d3=t.d3||{})})(this,function(t){"use strict";function n(t){return function(n,e){return ms(t(n),e)}}function e(t,n,e){var r=Math.abs(n-t)/Math.max(0,e),i=Math.pow(10,Math.floor(Math.log(r)/Math.LN10)),o=r/i;return o>=Rs?i*=10:o>=Us?i*=5:o>=Ds&&(i*=2),n<t?-i:i}function r(t){return t.length}function i(){}function o(t,n){var e=new i;if(t instanceof i)t.each(function(t,n){e.set(n,t)});else if(Array.isArray(t)){var r,o=-1,u=t.length;if(null==n)for(;++o<u;)e.set(o,t[o]);else for(;++o<u;)e.set(n(r=t[o],o,t),r)}else if(t)for(var a in t)e.set(a,t[a]);return e}function u(){return{}}function a(t,n,e){t[n]=e}function c(){return o()}function s(t,n,e){t.set(n,e)}function f(){}function l(t,n){var e=new f;if(t instanceof f)t.each(function(t){e.add(t)});else if(t){var r=-1,i=t.length;if(null==n)for(;++r<i;)e.add(t[r]);else for(;++r<i;)e.add(n(t[r],r,t))}return e}function h(t){return+t}function p(t){return t*t}function d(t){return t*(2-t)}function v(t){return((t*=2)<=1?t*t:--t*(2-t)+1)/2}function _(t){return t*t*t}function y(t){return--t*t*t+1}function g(t){return((t*=2)<=1?t*t*t:(t-=2)*t*t+2)/2}function m(t){return 1-Math.cos(t*xf)}function x(t){return Math.sin(t*xf)}function b(t){return(1-Math.cos(mf*t))/2}function w(t){return Math.pow(2,10*t-10)}function M(t){return 1-Math.pow(2,-10*t)}function T(t){return((t*=2)<=1?Math.pow(2,10*t-10):2-Math.pow(2,10-10*t))/2}function N(t){return 1-Math.sqrt(1-t*t)}function k(t){return Math.sqrt(1- --t*t)}function S(t){return((t*=2)<=1?1-Math.sqrt(1-t*t):Math.sqrt(1-(t-=2)*t)+1)/2}function A(t){return 1-E(1-t)}function E(t){return(t=+t)<bf?Cf*t*t:t<Mf?Cf*(t-=wf)*t+Tf:t<kf?Cf*(t-=Nf)*t+Sf:Cf*(t-=Af)*t+Ef}function C(t){return((t*=2)<=1?1-E(1-t):E(t-1)+1)/2}function z(t,n){return t[0]-n[0]||t[1]-n[1]}function P(t){for(var n=t.length,e=[0,1],r=2,i=2;i<n;++i){for(;r>1&&jf(t[e[r-2]],t[e[r-1]],t[i])<=0;)--r;e[r++]=i}return e.slice(0,r)}function q(){this._x0=this._y0=this._x1=this._y1=null,this._=[]}function L(){return new q}function R(t,n,e,r){if(isNaN(n)||isNaN(e))return t;var i,o,u,a,c,s,f,l,h,p=t._root,d={data:r},v=t._x0,_=t._y0,y=t._x1,g=t._y1;if(!p)return t._root=d,t;for(;p.length;)if((s=n>=(o=(v+y)/2))?v=o:y=o,(f=e>=(u=(_+g)/2))?_=u:g=u,i=p,!(p=p[l=f<<1|s]))return i[l]=d,t;if(a=+t._x.call(null,p.data),c=+t._y.call(null,p.data),n===a&&e===c)return d.next=p,i?i[l]=d:t._root=d,t;do i=i?i[l]=new Array(4):t._root=new Array(4),(s=n>=(o=(v+y)/2))?v=o:y=o,(f=e>=(u=(_+g)/2))?_=u:g=u;while((l=f<<1|s)===(h=(c>=u)<<1|a>=o));return i[h]=p,i[l]=d,t}function U(t){var n,e,r,i,o=t.length,u=new Array(o),a=new Array(o),c=1/0,s=1/0,f=-(1/0),l=-(1/0);for(e=0;e<o;++e)isNaN(r=+this._x.call(null,n=t[e]))||isNaN(i=+this._y.call(null,n))||(u[e]=r,a[e]=i,r<c&&(c=r),r>f&&(f=r),i<s&&(s=i),i>l&&(l=i));for(f<c&&(c=this._x0,f=this._x1),l<s&&(s=this._y0,l=this._y1),this.cover(c,s).cover(f,l),e=0;e<o;++e)R(this,u[e],a[e],t[e]);return this}function D(t){for(var n=0,e=t.length;n<e;++n)this.remove(t[n]);return this}function O(t){return t[0]}function F(t){return t[1]}function I(t,n,e){var r=new Y(null==n?O:n,null==e?F:e,NaN,NaN,NaN,NaN);return null==t?r:r.addAll(t)}function Y(t,n,e,r,i,o){this._x=t,this._y=n,this._x0=e,this._y0=r,this._x1=i,this._y1=o,this._root=void 0}function B(t){for(var n={data:t.data},e=n;t=t.next;)e=e.next={data:t.data};return n}function j(t){if(!(t>=1))throw new Error;this._size=t,this._call=this._error=null,this._tasks=[],this._data=[],this._waiting=this._active=this._ended=this._start=0}function H(t){if(!t._start)try{X(t)}catch(n){if(t._tasks[t._ended+t._active-1])W(t,n);else if(!t._data)throw n}}function X(t){for(;t._start=t._waiting&&t._active<t._size;){var n=t._ended+t._active,e=t._tasks[n],r=e.length-1,i=e[r];e[r]=V(t,n),--t._waiting,++t._active,e=i.apply(null,e),t._tasks[n]&&(t._tasks[n]=e||hl)}}function V(t,n){return function(e,r){t._tasks[n]&&(--t._active,++t._ended,t._tasks[n]=null,null==t._error&&(null!=e?W(t,e):(t._data[n]=r,t._waiting?H(t):$(t))))}}function W(t,n){var e,r=t._tasks.length;for(t._error=n,t._data=void 0,t._waiting=NaN;--r>=0;)if((e=t._tasks[r])&&(t._tasks[r]=null,e.abort))try{e.abort()}catch(t){}t._active=NaN,$(t)}function $(t){if(!t._active&&t._call){var n=t._data;t._data=void 0,t._call(t._error,n)}}function Z(t){return new j(arguments.length?+t:1/0)}function G(t){return t.innerRadius}function J(t){return t.outerRadius}function Q(t){return t.startAngle}function K(t){return t.endAngle}function tt(t){return t&&t.padAngle}function nt(t){return t>=1?_l:t<=-1?-_l:Math.asin(t)}function et(t,n,e,r,i,o,u,a){var c=e-t,s=r-n,f=u-i,l=a-o,h=(f*(n-o)-l*(t-i))/(l*c-f*s);return[t+h*c,n+h*s]}function rt(t,n,e,r,i,o,u){var a=t-e,c=n-r,s=(u?o:-o)/Math.sqrt(a*a+c*c),f=s*c,l=-s*a,h=t+f,p=n+l,d=e+f,v=r+l,_=(h+d)/2,y=(p+v)/2,g=d-h,m=v-p,x=g*g+m*m,b=i-o,w=h*v-d*p,M=(m<0?-1:1)*Math.sqrt(Math.max(0,b*b*x-w*w)),T=(w*m-g*M)/x,N=(-w*g-m*M)/x,k=(w*m+g*M)/x,S=(-w*g+m*M)/x,A=T-_,E=N-y,C=k-_,z=S-y;return A*A+E*E>C*C+z*z&&(T=k,N=S),{cx:T,cy:N,x01:-f,y01:-l,x11:T*(i/b-1),y11:N*(i/b-1)}}function it(t){this._context=t}function ot(t){return t[0]}function ut(t){return t[1]}function at(t){this._curve=t}function ct(t){function n(n){return new at(t(n))}return n._curve=t,n}function st(t){var n=t.curve;return t.angle=t.x,delete t.x,t.radius=t.y,delete t.y,t.curve=function(t){return arguments.length?n(ct(t)):n()._curve},t}function ft(t,n,e){t._context.bezierCurveTo((2*t._x0+t._x1)/3,(2*t._y0+t._y1)/3,(t._x0+2*t._x1)/3,(t._y0+2*t._y1)/3,(t._x0+4*t._x1+n)/6,(t._y0+4*t._y1+e)/6)}function lt(t){this._context=t}function ht(t){this._context=t}function pt(t){this._context=t}function dt(t,n){this._basis=new lt(t),this._beta=n}function vt(t,n,e){t._context.bezierCurveTo(t._x1+t._k*(t._x2-t._x0),t._y1+t._k*(t._y2-t._y0),t._x2+t._k*(t._x1-n),t._y2+t._k*(t._y1-e),t._x2,t._y2)}function _t(t,n){this._context=t,this._k=(1-n)/6}function yt(t,n){this._context=t,this._k=(1-n)/6}function gt(t,n){this._context=t,this._k=(1-n)/6}function mt(t,n,e){var r=t._x1,i=t._y1,o=t._x2,u=t._y2;if(t._l01_a>dl){var a=2*t._l01_2a+3*t._l01_a*t._l12_a+t._l12_2a,c=3*t._l01_a*(t._l01_a+t._l12_a);r=(r*a-t._x0*t._l12_2a+t._x2*t._l01_2a)/c,i=(i*a-t._y0*t._l12_2a+t._y2*t._l01_2a)/c}if(t._l23_a>dl){var s=2*t._l23_2a+3*t._l23_a*t._l12_a+t._l12_2a,f=3*t._l23_a*(t._l23_a+t._l12_a);o=(o*s+t._x1*t._l23_2a-n*t._l12_2a)/f,u=(u*s+t._y1*t._l23_2a-e*t._l12_2a)/f}t._context.bezierCurveTo(r,i,o,u,t._x2,t._y2)}function xt(t,n){this._context=t,this._alpha=n}function bt(t,n){this._context=t,this._alpha=n}function wt(t,n){this._context=t,this._alpha=n}function Mt(t){this._context=t}function Tt(t){return t<0?-1:1}function Nt(t,n,e){var r=t._x1-t._x0,i=n-t._x1,o=(t._y1-t._y0)/(r||i<0&&-0),u=(e-t._y1)/(i||r<0&&-0),a=(o*i+u*r)/(r+i);return(Tt(o)+Tt(u))*Math.min(Math.abs(o),Math.abs(u),.5*Math.abs(a))||0}function kt(t,n){var e=t._x1-t._x0;return e?(3*(t._y1-t._y0)/e-n)/2:n}function St(t,n,e){var r=t._x0,i=t._y0,o=t._x1,u=t._y1,a=(o-r)/3;t._context.bezierCurveTo(r+a,i+a*n,o-a,u-a*e,o,u)}function At(t){this._context=t}function Et(t){this._context=new Ct(t)}function Ct(t){this._context=t}function zt(t){return new At(t)}function Pt(t){return new Et(t)}function qt(t){this._context=t}function Lt(t){var n,e,r=t.length-1,i=new Array(r),o=new Array(r),u=new Array(r);for(i[0]=0,o[0]=2,u[0]=t[0]+2*t[1],n=1;n<r-1;++n)i[n]=1,o[n]=4,u[n]=4*t[n]+2*t[n+1];for(i[r-1]=2,o[r-1]=7,u[r-1]=8*t[r-1]+t[r],n=1;n<r;++n)e=i[n]/o[n-1],o[n]-=e,u[n]-=e*u[n-1];for(i[r-1]=u[r-1]/o[r-1],n=r-2;n>=0;--n)i[n]=(u[n]-i[n+1])/o[n];for(o[r-1]=(t[r]+i[r-1])/2,n=0;n<r-1;++n)o[n]=2*t[n+1]-i[n+1];return[i,o]}function Rt(t,n){this._context=t,this._t=n}function Ut(t){return new Rt(t,0)}function Dt(t){return new Rt(t,1)}function Ot(t,n){return t[n]}function Ft(t){for(var n,e=0,r=-1,i=t.length;++r<i;)(n=+t[r][1])&&(e+=n);return e}function It(t,n){var e=Object.create(t.prototype);for(var r in n)e[r]=n[r];return e}function Yt(){}function Bt(t){var n;return t=(t+"").trim().toLowerCase(),(n=wh.exec(t))?(n=parseInt(n[1],16),new Wt(n>>8&15|n>>4&240,n>>4&15|240&n,(15&n)<<4|15&n,1)):(n=Mh.exec(t))?jt(parseInt(n[1],16)):(n=Th.exec(t))?new Wt(n[1],n[2],n[3],1):(n=Nh.exec(t))?new Wt(255*n[1]/100,255*n[2]/100,255*n[3]/100,1):(n=kh.exec(t))?Ht(n[1],n[2],n[3],n[4]):(n=Sh.exec(t))?Ht(255*n[1]/100,255*n[2]/100,255*n[3]/100,n[4]):(n=Ah.exec(t))?$t(n[1],n[2]/100,n[3]/100,1):(n=Eh.exec(t))?$t(n[1],n[2]/100,n[3]/100,n[4]):Ch.hasOwnProperty(t)?jt(Ch[t]):"transparent"===t?new Wt(NaN,NaN,NaN,0):null}function jt(t){return new Wt(t>>16&255,t>>8&255,255&t,1)}function Ht(t,n,e,r){return r<=0&&(t=n=e=NaN),new Wt(t,n,e,r)}function Xt(t){return t instanceof Yt||(t=Bt(t)),t?(t=t.rgb(),new Wt(t.r,t.g,t.b,t.opacity)):new Wt}function Vt(t,n,e,r){return 1===arguments.length?Xt(t):new Wt(t,n,e,null==r?1:r)}function Wt(t,n,e,r){this.r=+t,this.g=+n,this.b=+e,this.opacity=+r}function $t(t,n,e,r){return r<=0?t=n=e=NaN:e<=0||e>=1?t=n=NaN:n<=0&&(t=NaN),new Jt(t,n,e,r)}function Zt(t){if(t instanceof Jt)return new Jt(t.h,t.s,t.l,t.opacity);if(t instanceof Yt||(t=Bt(t)),!t)return new Jt;if(t instanceof Jt)return t;t=t.rgb();var n=t.r/255,e=t.g/255,r=t.b/255,i=Math.min(n,e,r),o=Math.max(n,e,r),u=NaN,a=o-i,c=(o+i)/2;return a?(u=n===o?(e-r)/a+6*(e<r):e===o?(r-n)/a+2:(n-e)/a+4,a/=c<.5?o+i:2-o-i,u*=60):a=c>0&&c<1?0:u,new Jt(u,a,c,t.opacity)}function Gt(t,n,e,r){return 1===arguments.length?Zt(t):new Jt(t,n,e,null==r?1:r)}function Jt(t,n,e,r){this.h=+t,this.s=+n,this.l=+e,this.opacity=+r}function Qt(t,n,e){return 255*(t<60?n+(e-n)*t/60:t<180?e:t<240?n+(e-n)*(240-t)/60:n)}function Kt(t){if(t instanceof nn)return new nn(t.l,t.a,t.b,t.opacity);if(t instanceof sn){var n=t.h*zh;return new nn(t.l,Math.cos(n)*t.c,Math.sin(n)*t.c,t.opacity)}t instanceof Wt||(t=Xt(t));var e=un(t.r),r=un(t.g),i=un(t.b),o=en((.4124564*e+.3575761*r+.1804375*i)/Lh),u=en((.2126729*e+.7151522*r+.072175*i)/Rh),a=en((.0193339*e+.119192*r+.9503041*i)/Uh);return new nn(116*u-16,500*(o-u),200*(u-a),t.opacity)}function tn(t,n,e,r){return 1===arguments.length?Kt(t):new nn(t,n,e,null==r?1:r)}function nn(t,n,e,r){this.l=+t,this.a=+n,this.b=+e,this.opacity=+r}function en(t){return t>Ih?Math.pow(t,1/3):t/Fh+Dh}function rn(t){return t>Oh?t*t*t:Fh*(t-Dh)}function on(t){return 255*(t<=.0031308?12.92*t:1.055*Math.pow(t,1/2.4)-.055)}function un(t){return(t/=255)<=.04045?t/12.92:Math.pow((t+.055)/1.055,2.4)}function an(t){if(t instanceof sn)return new sn(t.h,t.c,t.l,t.opacity);t instanceof nn||(t=Kt(t));var n=Math.atan2(t.b,t.a)*Ph;return new sn(n<0?n+360:n,Math.sqrt(t.a*t.a+t.b*t.b),t.l,t.opacity)}function cn(t,n,e,r){return 1===arguments.length?an(t):new sn(t,n,e,null==r?1:r)}function sn(t,n,e,r){this.h=+t,this.c=+n,this.l=+e,this.opacity=+r}function fn(t){if(t instanceof hn)return new hn(t.h,t.s,t.l,t.opacity);t instanceof Wt||(t=Xt(t));var n=t.r/255,e=t.g/255,r=t.b/255,i=($h*r+Vh*n-Wh*e)/($h+Vh-Wh),o=r-i,u=(Xh*(e-i)-jh*o)/Hh,a=Math.sqrt(u*u+o*o)/(Xh*i*(1-i)),c=a?Math.atan2(u,o)*Ph-120:NaN;return new hn(c<0?c+360:c,a,i,t.opacity)}function ln(t,n,e,r){return 1===arguments.length?fn(t):new hn(t,n,e,null==r?1:r)}function hn(t,n,e,r){this.h=+t,this.s=+n,this.l=+e,this.opacity=+r}function pn(t,n,e,r,i){var o=t*t,u=o*t;return((1-3*t+3*o-u)*n+(4-6*o+3*u)*e+(1+3*t+3*o-3*u)*r+u*i)/6}function dn(t,n){return function(e){return t+e*n}}function vn(t,n,e){return t=Math.pow(t,e),n=Math.pow(n,e)-t,e=1/e,function(r){return Math.pow(t+r*n,e)}}function _n(t,n){var e=n-t;return e?dn(t,e>180||e<-180?e-360*Math.round(e/360):e):np(isNaN(t)?n:t)}function yn(t){return 1===(t=+t)?gn:function(n,e){return e-n?vn(n,e,t):np(isNaN(n)?e:n)}}function gn(t,n){var e=n-t;return e?dn(t,e):np(isNaN(t)?n:t)}function mn(t){return function(n){var e,r,i=n.length,o=new Array(i),u=new Array(i),a=new Array(i);for(e=0;e<i;++e)r=Vt(n[e]),o[e]=r.r||0,u[e]=r.g||0,a[e]=r.b||0;return o=t(o),u=t(u),a=t(a),r.opacity=1,function(t){return r.r=o(t),r.g=u(t),r.b=a(t),r+""}}}function xn(t){return function(){return t}}function bn(t){return function(n){return t(n)+""}}function wn(t){return"none"===t?vp:(Zh||(Zh=document.createElement("DIV"),Gh=document.documentElement,Jh=document.defaultView),Zh.style.transform=t,t=Jh.getComputedStyle(Gh.appendChild(Zh),null).getPropertyValue("transform"),Gh.removeChild(Zh),t=t.slice(7,-1).split(","),_p(+t[0],+t[1],+t[2],+t[3],+t[4],+t[5]))}function Mn(t){return null==t?vp:(Qh||(Qh=document.createElementNS("http://www.w3.org/2000/svg","g")),Qh.setAttribute("transform",t),(t=Qh.transform.baseVal.consolidate())?(t=t.matrix,_p(t.a,t.b,t.c,t.d,t.e,t.f)):vp)}function Tn(t,n,e,r){function i(t){return t.length?t.pop()+" ":""}function o(t,r,i,o,u,a){if(t!==i||r!==o){var c=u.push("translate(",null,n,null,e);a.push({i:c-4,x:ap(t,i)},{i:c-2,x:ap(r,o)})}else(i||o)&&u.push("translate("+i+n+o+e)}function u(t,n,e,o){t!==n?(t-n>180?n+=360:n-t>180&&(t+=360),o.push({i:e.push(i(e)+"rotate(",null,r)-2,x:ap(t,n)})):n&&e.push(i(e)+"rotate("+n+r)}function a(t,n,e,o){t!==n?o.push({i:e.push(i(e)+"skewX(",null,r)-2,x:ap(t,n)}):n&&e.push(i(e)+"skewX("+n+r)}function c(t,n,e,r,o,u){if(t!==e||n!==r){var a=o.push(i(o)+"scale(",null,",",null,")");u.push({i:a-4,x:ap(t,e)},{i:a-2,x:ap(n,r)})}else 1===e&&1===r||o.push(i(o)+"scale("+e+","+r+")")}return function(n,e){var r=[],i=[];return n=t(n),e=t(e),o(n.translateX,n.translateY,e.translateX,e.translateY,r,i),u(n.rotate,e.rotate,r,i),a(n.skewX,e.skewX,r,i),c(n.scaleX,n.scaleY,e.scaleX,e.scaleY,r,i),n=e=null,function(t){for(var n,e=-1,o=i.length;++e<o;)r[(n=i[e]).i]=n.x(t);return r.join("")}}}function Nn(t){return((t=Math.exp(t))+1/t)/2}function kn(t){return((t=Math.exp(t))-1/t)/2}function Sn(t){return((t=Math.exp(2*t))-1)/(t+1)}function An(t){return function(n,e){var r=t((n=Gt(n)).h,(e=Gt(e)).h),i=gn(n.s,e.s),o=gn(n.l,e.l),u=gn(n.opacity,e.opacity);return function(t){return n.h=r(t),n.s=i(t),n.l=o(t),n.opacity=u(t),n+""}}}function En(t,n){var e=gn((t=tn(t)).l,(n=tn(n)).l),r=gn(t.a,n.a),i=gn(t.b,n.b),o=gn(t.opacity,n.opacity);return function(n){return t.l=e(n),t.a=r(n),t.b=i(n),t.opacity=o(n),t+""}}function Cn(t){return function(n,e){var r=t((n=cn(n)).h,(e=cn(e)).h),i=gn(n.c,e.c),o=gn(n.l,e.l),u=gn(n.opacity,e.opacity);return function(t){return n.h=r(t),n.c=i(t),n.l=o(t),n.opacity=u(t),n+""}}}function zn(t){return function n(e){function r(n,r){var i=t((n=ln(n)).h,(r=ln(r)).h),o=gn(n.s,r.s),u=gn(n.l,r.l),a=gn(n.opacity,r.opacity);return function(t){return n.h=i(t),n.s=o(t),n.l=u(Math.pow(t,e)),n.opacity=a(t),n+""}}return e=+e,r.gamma=n,r}(1)}function Pn(){for(var t,n=0,e=arguments.length,r={};n<e;++n){if(!(t=arguments[n]+"")||t in r)throw new Error("illegal type: "+t);r[t]=[]}return new qn(r)}function qn(t){this._=t}function Ln(t,n){return t.trim().split(/^|\s+/).map(function(t){var e="",r=t.indexOf(".");if(r>=0&&(e=t.slice(r+1),t=t.slice(0,r)),t&&!n.hasOwnProperty(t))throw new Error("unknown type: "+t);return{type:t,name:e}})}function Rn(t,n){for(var e,r=0,i=t.length;r<i;++r)if((e=t[r]).name===n)return e.value}function Un(t,n,e){for(var r=0,i=t.length;r<i;++r)if(t[r].name===n){t[r]=zp,t=t.slice(0,r).concat(t.slice(r+1));break}return null!=e&&t.push({name:n,value:e}),t}function Dn(t){return new Function("d","return {"+t.map(function(t,n){return JSON.stringify(t)+": d["+n+"]"}).join(",")+"}")}function On(t,n){var e=Dn(t);return function(r,i){return n(e(r),i,t)}}function Fn(t){var n=Object.create(null),e=[];return t.forEach(function(t){for(var r in t)r in n||e.push(n[r]=r)}),e}function In(t){return function(n,e){t(null==n?e:null)}}function Yn(t){var n=t.responseType;return n&&"text"!==n?t.response:t.responseText}function Bn(t,n){return function(e){return t(e.responseText,n)}}function jn(){return od||(cd(Hn),od=ad.now()+ud)}function Hn(){od=0}function Xn(){this._call=this._time=this._next=null}function Vn(t,n,e){var r=new Xn;return r.restart(t,n,e),r}function Wn(){jn(),++td;for(var t,n=Pp;n;)(t=od-n._time)>=0&&n._call.call(null,t),n=n._next;--td}function $n(){od=(id=ad.now())+ud,td=nd=0;try{Wn()}finally{td=0,Gn(),od=0}}function Zn(){var t=ad.now(),n=t-id;n>rd&&(ud-=n,id=t)}function Gn(){for(var t,n,e=Pp,r=1/0;e;)e._call?(r>e._time&&(r=e._time),t=e,e=e._next):(n=e._next,e._next=null,e=t?t._next=n:Pp=n);qp=t,Jn(r)}function Jn(t){if(!td){nd&&(nd=clearTimeout(nd));var n=t-od;n>24?(t<1/0&&(nd=setTimeout($n,n)),ed&&(ed=clearInterval(ed))):(ed||(ed=setInterval(Zn,rd)),td=1,cd($n))}}function Qn(t,n,e,r){function i(n){return t(n=new Date(+n)),n}return i.floor=i,i.ceil=function(e){return t(e=new Date(e-1)),n(e,1),t(e),e},i.round=function(t){var n=i(t),e=i.ceil(t);return t-n<e-t?n:e},i.offset=function(t,e){return n(t=new Date(+t),null==e?1:Math.floor(e)),t},i.range=function(e,r,o){var u=[];if(e=i.ceil(e),o=null==o?1:Math.floor(o),!(e<r&&o>0))return u;do u.push(new Date(+e));while(n(e,o),t(e),e<r);return u},i.filter=function(e){return Qn(function(n){if(n>=n)for(;t(n),!e(n);)n.setTime(n-1)},function(t,r){if(t>=t)for(;--r>=0;)for(;n(t,1),!e(t););})},e&&(i.count=function(n,r){return ld.setTime(+n),hd.setTime(+r),t(ld),t(hd),Math.floor(e(ld,hd))},i.every=function(t){return t=Math.floor(t),isFinite(t)&&t>0?t>1?i.filter(r?function(n){return r(n)%t===0}:function(n){return i.count(0,n)%t===0}):i:null}),i}function Kn(t){return Qn(function(n){n.setDate(n.getDate()-(n.getDay()+7-t)%7),n.setHours(0,0,0,0)},function(t,n){t.setDate(t.getDate()+7*n)},function(t,n){return(n-t-(n.getTimezoneOffset()-t.getTimezoneOffset())*_d)/md})}function te(t){return Qn(function(n){n.setUTCDate(n.getUTCDate()-(n.getUTCDay()+7-t)%7),n.setUTCHours(0,0,0,0)},function(t,n){t.setUTCDate(t.getUTCDate()+7*n)},function(t,n){return(n-t)/md})}function ne(t){if(!(n=Tv.exec(t)))throw new Error("invalid format: "+t);var n,e=n[1]||" ",r=n[2]||">",i=n[3]||"-",o=n[4]||"",u=!!n[5],a=n[6]&&+n[6],c=!!n[7],s=n[8]&&+n[8].slice(1),f=n[9]||"";"n"===f?(c=!0,f="g"):Mv[f]||(f=""),(u||"0"===e&&"="===r)&&(u=!0,e="0",r="="),this.fill=e,this.align=r,this.sign=i,this.symbol=o,this.zero=u,this.width=a,this.comma=c,this.precision=s,this.type=f}function ee(t){return t}function re(n){return kv=Av(n),t.format=kv.format,t.formatPrefix=kv.formatPrefix,kv}function ie(t){if(0<=t.y&&t.y<100){var n=new Date(-1,t.m,t.d,t.H,t.M,t.S,t.L);return n.setFullYear(t.y),n}return new Date(t.y,t.m,t.d,t.H,t.M,t.S,t.L)}function oe(t){if(0<=t.y&&t.y<100){var n=new Date(Date.UTC(-1,t.m,t.d,t.H,t.M,t.S,t.L));return n.setUTCFullYear(t.y),n}return new Date(Date.UTC(t.y,t.m,t.d,t.H,t.M,t.S,t.L))}function ue(t){return{y:t,m:0,d:1,H:0,M:0,S:0,L:0}}function ae(t){function n(t,n){return function(e){var r,i,o,u=[],a=-1,c=0,s=t.length;for(e instanceof Date||(e=new Date(+e));++a<s;)37===t.charCodeAt(a)&&(u.push(t.slice(c,a)),null!=(i=qv[r=t.charAt(++a)])?r=t.charAt(++a):i="e"===r?" ":"0",(o=n[r])&&(r=o(e,i)),u.push(r),c=a+1);return u.push(t.slice(c,a)),u.join("")}}function e(t,n){return function(e){var i=ue(1900),o=r(i,t,e+="",0);if(o!=e.length)return null;if("p"in i&&(i.H=i.H%12+12*i.p),"W"in i||"U"in i){"w"in i||(i.w="W"in i?1:0);var u="Z"in i?oe(ue(i.y)).getUTCDay():n(ue(i.y)).getDay();i.m=0,i.d="W"in i?(i.w+6)%7+7*i.W-(u+5)%7:i.w+7*i.U-(u+6)%7}return"Z"in i?(i.H+=i.Z/100|0,i.M+=i.Z%100,oe(i)):n(i)}}function r(t,n,e,r){for(var i,o,u=0,a=n.length,c=e.length;u<a;){if(r>=c)return-1;if(i=n.charCodeAt(u++),37===i){if(i=n.charAt(u++),o=B[i in qv?n.charAt(u++):i],!o||(r=o(t,e,r))<0)return-1}else if(i!=e.charCodeAt(r++))return-1}return r}function i(t,n,e){var r=C.exec(n.slice(e));return r?(t.p=z[r[0].toLowerCase()],e+r[0].length):-1}function o(t,n,e){var r=L.exec(n.slice(e));return r?(t.w=R[r[0].toLowerCase()],e+r[0].length):-1}function u(t,n,e){var r=P.exec(n.slice(e));return r?(t.w=q[r[0].toLowerCase()],e+r[0].length):-1}function a(t,n,e){var r=O.exec(n.slice(e));return r?(t.m=F[r[0].toLowerCase()],e+r[0].length):-1}function c(t,n,e){var r=U.exec(n.slice(e));return r?(t.m=D[r[0].toLowerCase()],e+r[0].length):-1}function s(t,n,e){return r(t,w,n,e)}function f(t,n,e){return r(t,M,n,e)}function l(t,n,e){return r(t,T,n,e)}function h(t){return S[t.getDay()]}function p(t){return k[t.getDay()]}function d(t){return E[t.getMonth()]}function v(t){return A[t.getMonth()]}function _(t){return N[+(t.getHours()>=12)]}function y(t){return S[t.getUTCDay()]}function g(t){return k[t.getUTCDay()]}function m(t){return E[t.getUTCMonth()]}function x(t){return A[t.getUTCMonth()]}function b(t){return N[+(t.getUTCHours()>=12)]}var w=t.dateTime,M=t.date,T=t.time,N=t.periods,k=t.days,S=t.shortDays,A=t.months,E=t.shortMonths,C=fe(N),z=le(N),P=fe(k),q=le(k),L=fe(S),R=le(S),U=fe(A),D=le(A),O=fe(E),F=le(E),I={a:h,A:p,b:d,B:v,c:null,d:ke,e:ke,H:Se,I:Ae,j:Ee,L:Ce,m:ze,M:Pe,p:_,S:qe,U:Le,w:Re,W:Ue,x:null,X:null,y:De,Y:Oe,Z:Fe,"%":tr},Y={a:y,A:g,b:m,B:x,c:null,d:Ie,e:Ie,H:Ye,I:Be,j:je,L:He,m:Xe,M:Ve,p:b,S:We,U:$e,w:Ze,W:Ge,x:null,X:null,y:Je,Y:Qe,Z:Ke,"%":tr},B={a:o,A:u,b:a,B:c,c:s,d:me,e:me,H:be,I:be,j:xe,L:Te,m:ge,M:we,p:i,S:Me,U:pe,w:he,W:de,x:f,X:l,y:_e,Y:ve,Z:ye,"%":Ne};return I.x=n(M,I),I.X=n(T,I),I.c=n(w,I),Y.x=n(M,Y),Y.X=n(T,Y),Y.c=n(w,Y),{format:function(t){var e=n(t+="",I);return e.toString=function(){return t},e},parse:function(t){var n=e(t+="",ie);return n.toString=function(){return t},n},utcFormat:function(t){var e=n(t+="",Y);return e.toString=function(){return t},e},utcParse:function(t){var n=e(t,oe);return n.toString=function(){return t},n}}}function ce(t,n,e){var r=t<0?"-":"",i=(r?-t:t)+"",o=i.length;return r+(o<e?new Array(e-o+1).join(n)+i:i)}function se(t){return t.replace(Uv,"\\$&")}function fe(t){return new RegExp("^(?:"+t.map(se).join("|")+")","i")}function le(t){for(var n={},e=-1,r=t.length;++e<r;)n[t[e].toLowerCase()]=e;return n}function he(t,n,e){var r=Lv.exec(n.slice(e,e+1));return r?(t.w=+r[0],e+r[0].length):-1}function pe(t,n,e){var r=Lv.exec(n.slice(e));return r?(t.U=+r[0],e+r[0].length):-1}function de(t,n,e){var r=Lv.exec(n.slice(e));return r?(t.W=+r[0],e+r[0].length):-1}function ve(t,n,e){var r=Lv.exec(n.slice(e,e+4));return r?(t.y=+r[0],e+r[0].length):-1}function _e(t,n,e){var r=Lv.exec(n.slice(e,e+2));return r?(t.y=+r[0]+(+r[0]>68?1900:2e3),e+r[0].length):-1}function ye(t,n,e){var r=/^(Z)|([+-]\d\d)(?:\:?(\d\d))?/.exec(n.slice(e,e+6));return r?(t.Z=r[1]?0:-(r[2]+(r[3]||"00")),e+r[0].length):-1}function ge(t,n,e){var r=Lv.exec(n.slice(e,e+2));return r?(t.m=r[0]-1,e+r[0].length):-1}function me(t,n,e){var r=Lv.exec(n.slice(e,e+2));return r?(t.d=+r[0],e+r[0].length):-1}function xe(t,n,e){var r=Lv.exec(n.slice(e,e+3));return r?(t.m=0,t.d=+r[0],e+r[0].length):-1}function be(t,n,e){var r=Lv.exec(n.slice(e,e+2));return r?(t.H=+r[0],e+r[0].length):-1}function we(t,n,e){var r=Lv.exec(n.slice(e,e+2));return r?(t.M=+r[0],e+r[0].length):-1}function Me(t,n,e){var r=Lv.exec(n.slice(e,e+2));return r?(t.S=+r[0],e+r[0].length):-1}function Te(t,n,e){var r=Lv.exec(n.slice(e,e+3));return r?(t.L=+r[0],e+r[0].length):-1}function Ne(t,n,e){var r=Rv.exec(n.slice(e,e+1));return r?e+r[0].length:-1}function ke(t,n){return ce(t.getDate(),n,2)}function Se(t,n){return ce(t.getHours(),n,2)}function Ae(t,n){return ce(t.getHours()%12||12,n,2)}function Ee(t,n){return ce(1+kd.count(Hd(t),t),n,3)}function Ce(t,n){return ce(t.getMilliseconds(),n,3)}function ze(t,n){return ce(t.getMonth()+1,n,2)}function Pe(t,n){return ce(t.getMinutes(),n,2)}function qe(t,n){return ce(t.getSeconds(),n,2)}function Le(t,n){return ce(Ad.count(Hd(t),t),n,2)}function Re(t){return t.getDay()}function Ue(t,n){return ce(Ed.count(Hd(t),t),n,2)}function De(t,n){return ce(t.getFullYear()%100,n,2)}function Oe(t,n){return ce(t.getFullYear()%1e4,n,4)}function Fe(t){var n=t.getTimezoneOffset();return(n>0?"-":(n*=-1,"+"))+ce(n/60|0,"0",2)+ce(n%60,"0",2)}function Ie(t,n){return ce(t.getUTCDate(),n,2)}function Ye(t,n){return ce(t.getUTCHours(),n,2)}function Be(t,n){return ce(t.getUTCHours()%12||12,n,2)}function je(t,n){return ce(1+Gd.count(dv(t),t),n,3)}function He(t,n){return ce(t.getUTCMilliseconds(),n,3)}function Xe(t,n){return ce(t.getUTCMonth()+1,n,2)}function Ve(t,n){return ce(t.getUTCMinutes(),n,2)}function We(t,n){return ce(t.getUTCSeconds(),n,2)}function $e(t,n){return ce(Qd.count(dv(t),t),n,2)}function Ze(t){return t.getUTCDay()}function Ge(t,n){return ce(Kd.count(dv(t),t),n,2)}function Je(t,n){return ce(t.getUTCFullYear()%100,n,2)}function Qe(t,n){return ce(t.getUTCFullYear()%1e4,n,4)}function Ke(){return"+0000"}function tr(){return"%"}function nr(n){return Ev=ae(n),t.timeFormat=Ev.format,t.timeParse=Ev.parse,t.utcFormat=Ev.utcFormat,t.utcParse=Ev.utcParse,Ev}function er(t){return t.toISOString()}function rr(t){var n=new Date(t);return isNaN(n)?null:n}function ir(t){function n(n){var o=n+"",u=e.get(o);if(!u){if(i!==jv)return i;e.set(o,u=r.push(n))}return t[(u-1)%t.length]}var e=o(),r=[],i=jv;return t=null==t?[]:Bv.call(t),n.domain=function(t){if(!arguments.length)return r.slice();r=[],e=o();for(var i,u,a=-1,c=t.length;++a<c;)e.has(u=(i=t[a])+"")||e.set(u,r.push(i));return n},n.range=function(e){return arguments.length?(t=Bv.call(e),n):t.slice()},n.unknown=function(t){return arguments.length?(i=t,n):i},n.copy=function(){return ir().domain(r).range(t).unknown(i)},n}function or(){function t(){var t=i().length,r=u[1]<u[0],l=u[r-0],h=u[1-r];n=(h-l)/Math.max(1,t-c+2*s),a&&(n=Math.floor(n)),l+=(h-l-n*(t-c))*f,e=n*(1-c),a&&(l=Math.round(l),e=Math.round(e));var p=Ls(t).map(function(t){return l+n*t});return o(r?p.reverse():p)}var n,e,r=ir().unknown(void 0),i=r.domain,o=r.range,u=[0,1],a=!1,c=0,s=0,f=.5;return delete r.unknown,r.domain=function(n){return arguments.length?(i(n),t()):i()},r.range=function(n){return arguments.length?(u=[+n[0],+n[1]],t()):u.slice()},r.rangeRound=function(n){return u=[+n[0],+n[1]],a=!0,t()},r.bandwidth=function(){return e},r.step=function(){return n},r.round=function(n){return arguments.length?(a=!!n,t()):a},r.padding=function(n){return arguments.length?(c=s=Math.max(0,Math.min(1,n)),t()):c},r.paddingInner=function(n){return arguments.length?(c=Math.max(0,Math.min(1,n)),t()):c},r.paddingOuter=function(n){return arguments.length?(s=Math.max(0,Math.min(1,n)),t()):s},r.align=function(n){return arguments.length?(f=Math.max(0,Math.min(1,n)),t()):f},r.copy=function(){return or().domain(i()).range(u).round(a).paddingInner(c).paddingOuter(s).align(f)},t()}function ur(t){var n=t.copy;return t.padding=t.paddingOuter,delete t.paddingInner,delete t.paddingOuter,t.copy=function(){return ur(n())},t}function ar(){return ur(or().paddingInner(1))}function cr(t,n){return(n-=t=+t)?function(e){return(e-t)/n}:Hv(n)}function sr(t){return function(n,e){var r=t(n=+n,e=+e);return function(t){return t<=n?0:t>=e?1:r(t)}}}function fr(t){return function(n,e){var r=t(n=+n,e=+e);return function(t){return t<=0?n:t>=1?e:r(t)}}}function lr(t,n,e,r){var i=t[0],o=t[1],u=n[0],a=n[1];return o<i?(i=e(o,i),u=r(a,u)):(i=e(i,o),u=r(u,a)),function(t){return u(i(t))}}function hr(t,n,e,r){var i=Math.min(t.length,n.length)-1,o=new Array(i),u=new Array(i),a=-1;for(t[i]<t[0]&&(t=t.slice().reverse(),n=n.slice().reverse());++a<i;)o[a]=e(t[a],t[a+1]),u[a]=r(n[a],n[a+1]);return function(n){var e=ws(t,n,1,i)-1;return u[e](o[e](n))}}function pr(t,n){return n.domain(t.domain()).range(t.range()).interpolate(t.interpolate()).clamp(t.clamp())}function dr(t,n){function e(){return i=Math.min(a.length,c.length)>2?hr:lr,o=u=null,r}function r(n){return(o||(o=i(a,c,f?sr(t):t,s)))(+n)}var i,o,u,a=Vv,c=Vv,s=hp,f=!1;return r.invert=function(t){return(u||(u=i(c,a,cr,f?fr(n):n)))(+t)},r.domain=function(t){return arguments.length?(a=Yv.call(t,Xv),e()):a.slice()},r.range=function(t){return arguments.length?(c=Bv.call(t),e()):c.slice()},r.rangeRound=function(t){return c=Bv.call(t),s=pp,e()},r.clamp=function(t){return arguments.length?(f=!!t,e()):f},r.interpolate=function(t){return arguments.length?(s=t,e()):s},e()}function vr(t){var n=t.domain;return t.ticks=function(t){var e=n();return Os(e[0],e[e.length-1],null==t?10:t)},t.tickFormat=function(t,e){return Wv(n(),t,e)},t.nice=function(r){var i=n(),o=i.length-1,u=null==r?10:r,a=i[0],c=i[o],s=e(a,c,u);return s&&(s=e(Math.floor(a/s)*s,Math.ceil(c/s)*s,u),i[0]=Math.floor(a/s)*s,i[o]=Math.ceil(c/s)*s,n(i)),t},t}function _r(){var t=dr(cr,ap);return t.copy=function(){return pr(t,_r())},vr(t)}function yr(){function t(t){return+t}var n=[0,1];return t.invert=t,t.domain=t.range=function(e){return arguments.length?(n=Yv.call(e,Xv),t):n.slice()},t.copy=function(){return yr().domain(n)},vr(t)}function gr(t,n){return(n=Math.log(n/t))?function(e){return Math.log(e/t)/n}:Hv(n)}function mr(t,n){return t<0?function(e){return-Math.pow(-n,e)*Math.pow(-t,1-e)}:function(e){return Math.pow(n,e)*Math.pow(t,1-e)}}function xr(t){return isFinite(t)?+("1e"+t):t<0?0:t}function br(t){return 10===t?xr:t===Math.E?Math.exp:function(n){return Math.pow(t,n)}}function wr(t){return t===Math.E?Math.log:10===t&&Math.log10||2===t&&Math.log2||(t=Math.log(t),function(n){return Math.log(n)/t})}function Mr(t){return function(n){return-t(-n)}}function Tr(){function n(){return o=wr(i),u=br(i),r()[0]<0&&(o=Mr(o),u=Mr(u)),e}var e=dr(gr,mr).domain([1,10]),r=e.domain,i=10,o=wr(10),u=br(10);return e.base=function(t){return arguments.length?(i=+t,n()):i},e.domain=function(t){return arguments.length?(r(t),n()):r()},e.ticks=function(t){var n,e=r(),a=e[0],c=e[e.length-1];(n=c<a)&&(h=a,a=c,c=h);var s,f,l,h=o(a),p=o(c),d=null==t?10:+t,v=[];if(!(i%1)&&p-h<d){if(h=Math.round(h)-1,p=Math.round(p)+1,a>0){for(;h<p;++h)for(f=1,s=u(h);f<i;++f)if(l=s*f,!(l<a)){if(l>c)break;v.push(l)}}else for(;h<p;++h)for(f=i-1,s=u(h);f>=1;--f)if(l=s*f,!(l<a)){if(l>c)break;v.push(l)}}else v=Os(h,p,Math.min(p-h,d)).map(u);return n?v.reverse():v},e.tickFormat=function(n,r){if(null==r&&(r=10===i?".0e":","),"function"!=typeof r&&(r=t.format(r)),n===1/0)return r;null==n&&(n=10);var a=Math.max(1,i*n/e.ticks().length);return function(t){var n=t/u(Math.round(o(t)));return n*i<i-.5&&(n*=i),n<=a?r(t):""}},e.nice=function(){return r($v(r(),{floor:function(t){return u(Math.floor(o(t)))},ceil:function(t){return u(Math.ceil(o(t)))}}))},e.copy=function(){return pr(e,Tr().base(i))},e}function Nr(t,n){return t<0?-Math.pow(-t,n):Math.pow(t,n)}function kr(){function t(t,n){return(n=Nr(n,e)-(t=Nr(t,e)))?function(r){return(Nr(r,e)-t)/n}:Hv(n)}function n(t,n){return n=Nr(n,e)-(t=Nr(t,e)),function(r){return Nr(t+n*r,1/e)}}var e=1,r=dr(t,n),i=r.domain;return r.exponent=function(t){return arguments.length?(e=+t,i(i())):e},r.copy=function(){return pr(r,kr().exponent(e))},vr(r)}function Sr(){return kr().exponent(.5)}function Ar(){function t(){var t=0,o=Math.max(1,r.length);for(i=new Array(o-1);++t<o;)i[t-1]=Ys(e,t/o);return n}function n(t){if(!isNaN(t=+t))return r[ws(i,t)]}var e=[],r=[],i=[];return n.invertExtent=function(t){var n=r.indexOf(t);return n<0?[NaN,NaN]:[n>0?i[n-1]:e[0],n<i.length?i[n]:e[e.length-1]]},n.domain=function(n){if(!arguments.length)return e.slice();e=[];for(var r,i=0,o=n.length;i<o;++i)r=n[i],null==r||isNaN(r=+r)||e.push(r);return e.sort(ms),t()},n.range=function(n){return arguments.length?(r=Bv.call(n),t()):r.slice()},n.quantiles=function(){return i.slice()},n.copy=function(){return Ar().domain(e).range(r)},n}function Er(){function t(t){if(t<=t)return u[ws(o,t,0,i)]}function n(){var n=-1;for(o=new Array(i);++n<i;)o[n]=((n+1)*r-(n-i)*e)/(i+1);return t}var e=0,r=1,i=1,o=[.5],u=[0,1];return t.domain=function(t){return arguments.length?(e=+t[0],r=+t[1],n()):[e,r]},t.range=function(t){return arguments.length?(i=(u=Bv.call(t)).length-1,n()):u.slice()},t.invertExtent=function(t){var n=u.indexOf(t);return n<0?[NaN,NaN]:n<1?[e,o[0]]:n>=i?[o[i-1],r]:[o[n-1],o[n]]},t.copy=function(){return Er().domain([e,r]).range(u)},vr(t)}function Cr(){function t(t){if(t<=t)return e[ws(n,t,0,r)]}var n=[.5],e=[0,1],r=1;return t.domain=function(i){return arguments.length?(n=Bv.call(i),r=Math.min(n.length,e.length-1),t):n.slice()},t.range=function(i){return arguments.length?(e=Bv.call(i),r=Math.min(n.length,e.length-1),t):e.slice()},t.invertExtent=function(t){var r=e.indexOf(t);return[n[r-1],n[r]]},t.copy=function(){return Cr().domain(n).range(e)},t}function zr(t){return new Date(t);
}function Pr(t){return t instanceof Date?+t:+new Date(+t)}function qr(t,n,r,i,o,u,a,c,s){function f(e){return(a(e)<e?v:u(e)<e?_:o(e)<e?y:i(e)<e?g:n(e)<e?r(e)<e?m:x:t(e)<e?b:w)(e)}function l(n,r,i,o){if(null==n&&(n=10),"number"==typeof n){var u=Math.abs(i-r)/n,a=xs(function(t){return t[2]}).right(M,u);a===M.length?(o=e(r/n_,i/n_,n),n=t):a?(a=M[u/M[a-1][2]<M[a][2]/u?a-1:a],o=a[1],n=a[0]):(o=e(r,i,n),n=c)}return null==o?n:n.every(o)}var h=dr(cr,ap),p=h.invert,d=h.domain,v=s(".%L"),_=s(":%S"),y=s("%I:%M"),g=s("%I %p"),m=s("%a %d"),x=s("%b %d"),b=s("%B"),w=s("%Y"),M=[[a,1,Zv],[a,5,5*Zv],[a,15,15*Zv],[a,30,30*Zv],[u,1,Gv],[u,5,5*Gv],[u,15,15*Gv],[u,30,30*Gv],[o,1,Jv],[o,3,3*Jv],[o,6,6*Jv],[o,12,12*Jv],[i,1,Qv],[i,2,2*Qv],[r,1,Kv],[n,1,t_],[n,3,3*t_],[t,1,n_]];return h.invert=function(t){return new Date(p(t))},h.domain=function(t){return arguments.length?d(Yv.call(t,Pr)):d().map(zr)},h.ticks=function(t,n){var e,r=d(),i=r[0],o=r[r.length-1],u=o<i;return u&&(e=i,i=o,o=e),e=l(t,i,o,n),e=e?e.range(i,o+1):[],u?e.reverse():e},h.tickFormat=function(t,n){return null==n?f:s(n)},h.nice=function(t,n){var e=d();return(t=l(t,e[0],e[e.length-1],n))?d($v(e,t)):h},h.copy=function(){return pr(h,qr(t,n,r,i,o,u,a,c,s))},h}function Lr(t){var n=t.length;return function(e){return t[Math.max(0,Math.min(n-1,Math.floor(e*n)))]}}function Rr(t){function n(n){var o=(n-e)/(r-e);return t(i?Math.max(0,Math.min(1,o)):o)}var e=0,r=1,i=!1;return n.domain=function(t){return arguments.length?(e=+t[0],r=+t[1],n):[e,r]},n.clamp=function(t){return arguments.length?(i=!!t,n):i},n.interpolator=function(e){return arguments.length?(t=e,n):t},n.copy=function(){return Rr(t).domain([e,r]).clamp(i)},vr(n)}function Ur(t){return function(){var n=this.ownerDocument,e=this.namespaceURI;return e===g_&&n.documentElement.namespaceURI===g_?n.createElement(t):n.createElementNS(e,t)}}function Dr(t){return function(){return this.ownerDocument.createElementNS(t.space,t.local)}}function Or(){return new Fr}function Fr(){this._="@"+(++w_).toString(36)}function Ir(t,n,e){return t=Yr(t,n,e),function(n){var e=n.relatedTarget;e&&(e===this||8&e.compareDocumentPosition(this))||t.call(this,n)}}function Yr(n,e,r){return function(i){var o=t.event;t.event=i;try{n.call(this,this.__data__,e,r)}finally{t.event=o}}}function Br(t){return t.trim().split(/^|\s+/).map(function(t){var n="",e=t.indexOf(".");return e>=0&&(n=t.slice(e+1),t=t.slice(0,e)),{type:t,name:n}})}function jr(t){return function(){var n=this.__on;if(n){for(var e,r=0,i=-1,o=n.length;r<o;++r)e=n[r],t.type&&e.type!==t.type||e.name!==t.name?n[++i]=e:this.removeEventListener(e.type,e.listener,e.capture);++i?n.length=i:delete this.__on}}}function Hr(t,n,e){var r=S_.hasOwnProperty(t.type)?Ir:Yr;return function(i,o,u){var a,c=this.__on,s=r(n,o,u);if(c)for(var f=0,l=c.length;f<l;++f)if((a=c[f]).type===t.type&&a.name===t.name)return this.removeEventListener(a.type,a.listener,a.capture),this.addEventListener(a.type,a.listener=s,a.capture=e),void(a.value=n);this.addEventListener(t.type,s,e),a={type:t.type,name:t.name,value:n,listener:s,capture:e},c?c.push(a):this.__on=[a]}}function Xr(n,e,r,i){var o=t.event;n.sourceEvent=t.event,t.event=n;try{return e.apply(r,i)}finally{t.event=o}}function Vr(){}function Wr(){return[]}function $r(t,n){this.ownerDocument=t.ownerDocument,this.namespaceURI=t.namespaceURI,this._next=null,this._parent=t,this.__data__=n}function Zr(t,n,e,r,i,o){for(var u,a=0,c=n.length,s=o.length;a<s;++a)(u=n[a])?(u.__data__=o[a],r[a]=u):e[a]=new $r(t,o[a]);for(;a<c;++a)(u=n[a])&&(i[a]=u)}function Gr(t,n,e,r,i,o,u){var a,c,s,f={},l=n.length,h=o.length,p=new Array(l);for(a=0;a<l;++a)(c=n[a])&&(p[a]=s=Y_+u.call(c,c.__data__,a,n),s in f?i[a]=c:f[s]=c);for(a=0;a<h;++a)s=Y_+u.call(t,o[a],a,o),(c=f[s])?(r[a]=c,c.__data__=o[a],f[s]=null):e[a]=new $r(t,o[a]);for(a=0;a<l;++a)(c=n[a])&&f[p[a]]===c&&(i[a]=c)}function Jr(t,n){return t<n?-1:t>n?1:t>=n?0:NaN}function Qr(t){return function(){this.removeAttribute(t)}}function Kr(t){return function(){this.removeAttributeNS(t.space,t.local)}}function ti(t,n){return function(){this.setAttribute(t,n)}}function ni(t,n){return function(){this.setAttributeNS(t.space,t.local,n)}}function ei(t,n){return function(){var e=n.apply(this,arguments);null==e?this.removeAttribute(t):this.setAttribute(t,e)}}function ri(t,n){return function(){var e=n.apply(this,arguments);null==e?this.removeAttributeNS(t.space,t.local):this.setAttributeNS(t.space,t.local,e)}}function ii(t){return function(){this.style.removeProperty(t)}}function oi(t,n,e){return function(){this.style.setProperty(t,n,e)}}function ui(t,n,e){return function(){var r=n.apply(this,arguments);null==r?this.style.removeProperty(t):this.style.setProperty(t,r,e)}}function ai(t){return function(){delete this[t]}}function ci(t,n){return function(){this[t]=n}}function si(t,n){return function(){var e=n.apply(this,arguments);null==e?delete this[t]:this[t]=e}}function fi(t){return t.trim().split(/^|\s+/)}function li(t){return t.classList||new hi(t)}function hi(t){this._node=t,this._names=fi(t.getAttribute("class")||"")}function pi(t,n){for(var e=li(t),r=-1,i=n.length;++r<i;)e.add(n[r])}function di(t,n){for(var e=li(t),r=-1,i=n.length;++r<i;)e.remove(n[r])}function vi(t){return function(){pi(this,t)}}function _i(t){return function(){di(this,t)}}function yi(t,n){return function(){(n.apply(this,arguments)?pi:di)(this,t)}}function gi(){this.textContent=""}function mi(t){return function(){this.textContent=t}}function xi(t){return function(){var n=t.apply(this,arguments);this.textContent=null==n?"":n}}function bi(){this.innerHTML=""}function wi(t){return function(){this.innerHTML=t}}function Mi(t){return function(){var n=t.apply(this,arguments);this.innerHTML=null==n?"":n}}function Ti(){this.nextSibling&&this.parentNode.appendChild(this)}function Ni(){this.previousSibling&&this.parentNode.insertBefore(this,this.parentNode.firstChild)}function ki(){return null}function Si(){var t=this.parentNode;t&&t.removeChild(this)}function Ai(t,n,e){var r=ty(t),i=r.CustomEvent;i?i=new i(n,e):(i=r.document.createEvent("Event"),e?(i.initEvent(n,e.bubbles,e.cancelable),i.detail=e.detail):i.initEvent(n,!1,!1)),t.dispatchEvent(i)}function Ei(t,n){return function(){return Ai(this,t,n)}}function Ci(t,n){return function(){return Ai(this,t,n.apply(this,arguments))}}function zi(t,n){this._groups=t,this._parents=n}function Pi(){return new zi([[document.documentElement]],py)}function qi(t,n){var e=t.__transition;if(!e||!(e=e[n])||e.state>xy)throw new Error("too late");return e}function Li(t,n){var e=t.__transition;if(!e||!(e=e[n])||e.state>wy)throw new Error("too late");return e}function Ri(t,n){var e=t.__transition;if(!e||!(e=e[n]))throw new Error("too late");return e}function Ui(t,n,e){function r(t){e.state=by,e.timer.restart(i,e.delay,e.time),e.delay<=t&&i(t-e.delay)}function i(r){var s,f,l,h;if(e.state!==by)return u();for(s in c)if(h=c[s],h.name===e.name){if(h.state===My)return sd(i);h.state===Ty?(h.state=ky,h.timer.stop(),h.on.call("interrupt",t,t.__data__,h.index,h.group),delete c[s]):+s<n&&(h.state=ky,h.timer.stop(),delete c[s])}if(sd(function(){e.state===My&&(e.state=Ty,e.timer.restart(o,e.delay,e.time),o(r))}),e.state=wy,e.on.call("start",t,t.__data__,e.index,e.group),e.state===wy){for(e.state=My,a=new Array(l=e.tween.length),s=0,f=-1;s<l;++s)(h=e.tween[s].value.call(t,t.__data__,e.index,e.group))&&(a[++f]=h);a.length=f+1}}function o(n){for(var r=n<e.duration?e.ease.call(null,n/e.duration):(e.timer.restart(u),e.state=Ny,1),i=-1,o=a.length;++i<o;)a[i].call(null,r);e.state===Ny&&(e.on.call("end",t,t.__data__,e.index,e.group),u())}function u(){e.state=ky,e.timer.stop(),delete c[n];for(var r in c)return;delete t.__transition}var a,c=t.__transition;c[n]=e,e.timer=Vn(r,0,e.time)}function Di(t,n){var e,r;return function(){var i=Li(this,t),o=i.tween;if(o!==e){r=e=o;for(var u=0,a=r.length;u<a;++u)if(r[u].name===n){r=r.slice(),r.splice(u,1);break}}i.tween=r}}function Oi(t,n,e){var r,i;if("function"!=typeof e)throw new Error;return function(){var o=Li(this,t),u=o.tween;if(u!==r){i=(r=u).slice();for(var a={name:n,value:e},c=0,s=i.length;c<s;++c)if(i[c].name===n){i[c]=a;break}c===s&&i.push(a)}o.tween=i}}function Fi(t,n,e){var r=t._id;return t.each(function(){var t=Li(this,r);(t.value||(t.value={}))[n]=e.apply(this,arguments)}),function(t){return Ri(t,r).value[n]}}function Ii(t){return function(){this.removeAttribute(t)}}function Yi(t){return function(){this.removeAttributeNS(t.space,t.local)}}function Bi(t,n,e){var r,i;return function(){var o=this.getAttribute(t);return o===e?null:o===r?i:i=n(r=o,e)}}function ji(t,n,e){var r,i;return function(){var o=this.getAttributeNS(t.space,t.local);return o===e?null:o===r?i:i=n(r=o,e)}}function Hi(t,n,e){var r,i,o;return function(){var u,a=e(this);return null==a?void this.removeAttribute(t):(u=this.getAttribute(t),u===a?null:u===r&&a===i?o:o=n(r=u,i=a))}}function Xi(t,n,e){var r,i,o;return function(){var u,a=e(this);return null==a?void this.removeAttributeNS(t.space,t.local):(u=this.getAttributeNS(t.space,t.local),u===a?null:u===r&&a===i?o:o=n(r=u,i=a))}}function Vi(t,n){function e(){var e=this,r=n.apply(e,arguments);return r&&function(n){e.setAttributeNS(t.space,t.local,r(n))}}return e._value=n,e}function Wi(t,n){function e(){var e=this,r=n.apply(e,arguments);return r&&function(n){e.setAttribute(t,r(n))}}return e._value=n,e}function $i(t,n){return function(){qi(this,t).delay=+n.apply(this,arguments)}}function Zi(t,n){return n=+n,function(){qi(this,t).delay=n}}function Gi(t,n){return function(){Li(this,t).duration=+n.apply(this,arguments)}}function Ji(t,n){return n=+n,function(){Li(this,t).duration=n}}function Qi(t,n){if("function"!=typeof n)throw new Error;return function(){Li(this,t).ease=n}}function Ki(t){return(t+"").trim().split(/^|\s+/).every(function(t){var n=t.indexOf(".");return n>=0&&(t=t.slice(0,n)),!t||"start"===t})}function to(t,n,e){var r,i,o=Ki(n)?qi:Li;return function(){var u=o(this,t),a=u.on;a!==r&&(i=(r=a).copy()).on(n,e),u.on=i}}function no(t){return function(){var n=this.parentNode;for(var e in this.__transition)if(+e!==t)return;n&&n.removeChild(this)}}function eo(t,n){var e,r,i;return function(){var o=ty(this).getComputedStyle(this,null),u=o.getPropertyValue(t),a=(this.style.removeProperty(t),o.getPropertyValue(t));return u===a?null:u===e&&a===r?i:i=n(e=u,r=a)}}function ro(t){return function(){this.style.removeProperty(t)}}function io(t,n,e){var r,i;return function(){var o=ty(this).getComputedStyle(this,null).getPropertyValue(t);return o===e?null:o===r?i:i=n(r=o,e)}}function oo(t,n,e){var r,i,o;return function(){var u=ty(this).getComputedStyle(this,null),a=u.getPropertyValue(t),c=e(this);return null==c&&(this.style.removeProperty(t),c=u.getPropertyValue(t)),a===c?null:a===r&&c===i?o:o=n(r=a,i=c)}}function uo(t,n,e){function r(){var r=this,i=n.apply(r,arguments);return i&&function(n){r.style.setProperty(t,i(n),e)}}return r._value=n,r}function ao(t){return function(){this.textContent=t}}function co(t){return function(){var n=t(this);this.textContent=null==n?"":n}}function so(t,n,e,r){this._groups=t,this._parents=n,this._name=e,this._id=r}function fo(t){return Pi().transition(t)}function lo(){return++Zy}function ho(t,n){for(var e;!(e=t.__transition)||!(e=e[n]);)if(!(t=t.parentNode))return Jy.time=jn(),Jy;return e}function po(t,n,e){var r=t(e);return"translate("+(isFinite(r)?r:n(e))+",0)"}function vo(t,n,e){var r=t(e);return"translate(0,"+(isFinite(r)?r:n(e))+")"}function _o(t){var n=t.bandwidth()/2;return t.round()&&(n=Math.round(n)),function(e){return t(e)+n}}function yo(){return!this.__axis}function go(t,n){function e(e){var s,f=null==i?n.ticks?n.ticks.apply(n,r):n.domain():i,l=null==o?n.tickFormat?n.tickFormat.apply(n,r):eg:o,h=Math.max(u,0)+c,p=t===rg||t===og?po:vo,d=n.range(),v=d[0]+.5,_=d[d.length-1]+.5,y=(n.bandwidth?_o:eg)(n.copy()),g=e.selection?e.selection():e,m=g.selectAll(".domain").data([null]),x=g.selectAll(".tick").data(f,n).order(),b=x.exit(),w=x.enter().append("g").attr("class","tick"),M=x.select("line"),T=x.select("text"),N=t===rg||t===ug?-1:1,k=t===ug||t===ig?(s="x","y"):(s="y","x");m=m.merge(m.enter().insert("path",".tick").attr("class","domain").attr("stroke","#000")),x=x.merge(w),M=M.merge(w.append("line").attr("stroke","#000").attr(s+"2",N*u).attr(k+"1",.5).attr(k+"2",.5)),T=T.merge(w.append("text").attr("fill","#000").attr(s,N*h).attr(k,.5).attr("dy",t===rg?"0em":t===og?"0.71em":"0.32em")),e!==g&&(m=m.transition(e),x=x.transition(e),M=M.transition(e),T=T.transition(e),b=b.transition(e).attr("opacity",ag).attr("transform",function(t){return p(y,this.parentNode.__axis||y,t)}),w.attr("opacity",ag).attr("transform",function(t){return p(this.parentNode.__axis||y,y,t)})),b.remove(),m.attr("d",t===ug||t==ig?"M"+N*a+","+v+"H0.5V"+_+"H"+N*a:"M"+v+","+N*a+"V0.5H"+_+"V"+N*a),x.attr("opacity",1).attr("transform",function(t){return p(y,y,t)}),M.attr(s+"2",N*u),T.attr(s,N*h).text(l),g.filter(yo).attr("fill","none").attr("font-size",10).attr("font-family","sans-serif").attr("text-anchor",t===ig?"start":t===ug?"end":"middle"),g.each(function(){this.__axis=y})}var r=[],i=null,o=null,u=6,a=6,c=3;return e.scale=function(t){return arguments.length?(n=t,e):n},e.ticks=function(){return r=ng.call(arguments),e},e.tickArguments=function(t){return arguments.length?(r=null==t?[]:ng.call(t),e):r.slice()},e.tickValues=function(t){return arguments.length?(i=null==t?null:ng.call(t),e):i&&i.slice()},e.tickFormat=function(t){return arguments.length?(o=t,e):o},e.tickSize=function(t){return arguments.length?(u=a=+t,e):u},e.tickSizeInner=function(t){return arguments.length?(u=+t,e):u},e.tickSizeOuter=function(t){return arguments.length?(a=+t,e):a},e.tickPadding=function(t){return arguments.length?(c=+t,e):c},e}function mo(t){return go(rg,t)}function xo(t){return go(ig,t)}function bo(t){return go(og,t)}function wo(t){return go(ug,t)}function Mo(t,n){return t.parent===n.parent?1:2}function To(t){return t.reduce(No,0)/t.length}function No(t,n){return t+n.x}function ko(t){return 1+t.reduce(So,0)}function So(t,n){return Math.max(t,n.y)}function Ao(t){for(var n;n=t.children;)t=n[0];return t}function Eo(t){for(var n;n=t.children;)t=n[n.length-1];return t}function Co(t,n){if(t===n)return t;var e=t.ancestors(),r=n.ancestors(),i=null;for(t=e.pop(),n=r.pop();t===n;)i=t,t=e.pop(),n=r.pop();return i}function zo(t,n){var e,r,i,o,u,a=new Uo(t),c=+t.value&&(a.value=t.value),s=[a];for(null==n&&(n=qo);e=s.pop();)if(c&&(e.value=+e.data.value),(i=n(e.data))&&(u=i.length))for(e.children=new Array(u),o=u-1;o>=0;--o)s.push(r=e.children[o]=new Uo(i[o])),r.parent=e,r.depth=e.depth+1;return a.eachBefore(Ro)}function Po(){return zo(this).eachBefore(Lo)}function qo(t){return t.children}function Lo(t){t.data=t.data.data}function Ro(t){var n=0;do t.height=n;while((t=t.parent)&&t.height<++n)}function Uo(t){this.data=t,this.depth=this.height=0,this.parent=null}function Do(t){this._=t,this.next=null}function Oo(t,n){var e=n.x-t.x,r=n.y-t.y,i=t.r-n.r;return i*i+1e-6>e*e+r*r}function Fo(t,n){var e,r,i,o=null,u=t.head;switch(n.length){case 1:e=Io(n[0]);break;case 2:e=Yo(n[0],n[1]);break;case 3:e=Bo(n[0],n[1],n[2])}for(;u;)i=u._,r=u.next,e&&Oo(e,i)?o=u:(o?(t.tail=o,o.next=null):t.head=t.tail=null,n.push(i),e=Fo(t,n),n.pop(),t.head?(u.next=t.head,t.head=u):(u.next=null,t.head=t.tail=u),o=t.tail,o.next=r),u=r;return t.tail=o,e}function Io(t){return{x:t.x,y:t.y,r:t.r}}function Yo(t,n){var e=t.x,r=t.y,i=t.r,o=n.x,u=n.y,a=n.r,c=o-e,s=u-r,f=a-i,l=Math.sqrt(c*c+s*s);return{x:(e+o+c/l*f)/2,y:(r+u+s/l*f)/2,r:(l+i+a)/2}}function Bo(t,n,e){var r=t.x,i=t.y,o=t.r,u=n.x,a=n.y,c=n.r,s=e.x,f=e.y,l=e.r,h=2*(r-u),p=2*(i-a),d=2*(c-o),v=r*r+i*i-o*o-u*u-a*a+c*c,_=2*(r-s),y=2*(i-f),g=2*(l-o),m=r*r+i*i-o*o-s*s-f*f+l*l,x=_*p-h*y,b=(p*m-y*v)/x-r,w=(y*d-p*g)/x,M=(_*v-h*m)/x-i,T=(h*g-_*d)/x,N=w*w+T*T-1,k=2*(b*w+M*T+o),S=b*b+M*M-o*o,A=(-k-Math.sqrt(k*k-4*N*S))/(2*N);return{x:b+w*A+r,y:M+T*A+i,r:A}}function jo(t,n,e){var r=t.x,i=t.y,o=n.r+e.r,u=t.r+e.r,a=n.x-r,c=n.y-i,s=a*a+c*c;if(s){var f=.5+((u*=u)-(o*=o))/(2*s),l=Math.sqrt(Math.max(0,2*o*(u+s)-(u-=s)*u-o*o))/(2*s);e.x=r+f*a+l*c,e.y=i+f*c-l*a}else e.x=r+u,e.y=i}function Ho(t,n){var e=n.x-t.x,r=n.y-t.y,i=t.r+n.r;return i*i>e*e+r*r}function Xo(t,n,e){var r=t.x-n,i=t.y-e;return r*r+i*i}function Vo(t){this._=t,this.next=null,this.previous=null}function Wo(t){if(!(i=t.length))return 0;var n,e,r,i;if(n=t[0],n.x=0,n.y=0,!(i>1))return n.r;if(e=t[1],n.x=-e.r,e.x=n.r,e.y=0,!(i>2))return n.r+e.r;jo(e,n,r=t[2]);var o,u,a,c,s,f,l,h=n.r*n.r,p=e.r*e.r,d=r.r*r.r,v=h+p+d,_=h*n.x+p*e.x+d*r.x,y=h*n.y+p*e.y+d*r.y;n=new Vo(n),e=new Vo(e),r=new Vo(r),n.next=r.previous=e,e.next=n.previous=r,r.next=e.previous=n;t:for(a=3;a<i;++a){if(jo(n._,e._,r=t[a]),r=new Vo(r),(s=n.previous)===(c=e.next)){if(Ho(c._,r._)){n=e,e=c,--a;continue t}}else{f=c._.r,l=s._.r;do if(f<=l){if(Ho(c._,r._)){e=c,n.next=e,e.previous=n,--a;continue t}c=c.next,f+=c._.r}else{if(Ho(s._,r._)){n=s,n.next=e,e.previous=n,--a;continue t}s=s.previous,l+=s._.r}while(c!==s.next)}for(r.previous=n,r.next=e,n.next=e.previous=e=r,v+=d=r._.r*r._.r,_+=d*r._.x,y+=d*r._.y,h=Xo(n._,o=_/v,u=y/v);(r=r.next)!==e;)(d=Xo(r._,o,u))<h&&(n=r,h=d);e=n.next}for(n=[e._],r=e;(r=r.next)!==e;)n.push(r._);for(r=xg(n),a=0;a<i;++a)n=t[a],n.x-=r.x,n.y-=r.y;return r.r}function $o(t){return null==t?null:Zo(t)}function Zo(t){if("function"!=typeof t)throw new Error;return t}function Go(){return 0}function Jo(t){return Math.sqrt(t.value)}function Qo(t){return function(n){n.children||(n.r=Math.max(0,+t(n)||0))}}function Ko(t,n){return function(e){if(r=e.children){var r,i,o,u=r.length,a=t(e)*n||0;if(a)for(i=0;i<u;++i)r[i].r+=a;if(o=Wo(r),a)for(i=0;i<u;++i)r[i].r-=a;e.r=o+a}}}function tu(t){return function(n){var e=n.parent;n.r*=t,e&&(n.x=e.x+t*n.x,n.y=e.y+t*n.y)}}function nu(t){return t.id}function eu(t){return t.parentId}function ru(t,n){return t.parent===n.parent?1:2}function iu(t){var n=t.children;return n?n[0]:t.t}function ou(t){var n=t.children;return n?n[n.length-1]:t.t}function uu(t,n,e){var r=e/(n.i-t.i);n.c-=r,n.s+=e,t.c+=r,n.z+=e,n.m+=e}function au(t){for(var n,e=0,r=0,i=t.children,o=i.length;--o>=0;)n=i[o],n.z+=e,n.m+=e,e+=n.s+(r+=n.c)}function cu(t,n,e){return t.a.parent===n.parent?t.a:e}function su(t,n){this._=t,this.parent=null,this.children=null,this.A=null,this.a=this,this.z=0,this.m=0,this.c=0,this.s=0,this.t=null,this.i=n}function fu(t){for(var n,e,r,i,o,u=new su(t,0),a=[u];n=a.pop();)if(r=n._.children)for(n.children=new Array(o=r.length),i=o-1;i>=0;--i)a.push(e=n.children[i]=new su(r[i],i)),e.parent=n;return(u.parent=new su(null,0)).children=[u],u}function lu(t,n,e,r,i,o){for(var u,a,c,s,f,l,h,p,d,v,_,y,g=[],m=n.children,x=0,b=m.length,w=n.value;x<b;){for(s=i-e,f=o-r,h=p=l=m[x].value,_=Math.max(f/s,s/f)/(w*t),y=l*l*_,v=Math.max(p/y,y/h),c=x+1;c<b;++c){if(l+=a=m[c].value,a<h&&(h=a),a>p&&(p=a),y=l*l*_,d=Math.max(p/y,y/h),d>v){l-=a;break}v=d}g.push(u={value:l,dice:s<f,children:m.slice(x,c)}),u.dice?Ng(u,e,r,i,w?r+=f*l/w:o):Pg(u,e,r,w?e+=s*l/w:i,o),w-=l,x=c}return g}function hu(t){return t.x+t.vx}function pu(t){return t.y+t.vy}function du(t,n){return n}function vu(t){return t.x}function _u(t){return t.y}function yu(){t.event.stopImmediatePropagation()}function gu(t,n){var e=t.document.documentElement,r=dy(t).on("dragstart.drag",null);n&&(r.on("click.drag",Gg,!0),setTimeout(function(){r.on("click.drag",null)},0)),"onselectstart"in e?r.on("selectstart.drag",null):(e.style.MozUserSelect=e.__noselect,delete e.__noselect)}function mu(t,n,e,r,i,o,u,a,c,s){this.target=t,this.type=n,this.subject=e,this.identifier=r,this.active=i,this.x=o,this.y=u,this.dx=a,this.dy=c,this._=s}function xu(){return!t.event.button}function bu(){return this.parentNode}function wu(n){return null==n?{x:t.event.x,y:t.event.y}:n}function Mu(t){return t[0]}function Tu(t){return t[1]}function Nu(){this._=null}function ku(t){t.U=t.C=t.L=t.R=t.P=t.N=null}function Su(t,n){var e=n,r=n.R,i=e.U;i?i.L===e?i.L=r:i.R=r:t._=r,r.U=i,e.U=r,e.R=r.L,e.R&&(e.R.U=e),r.L=e}function Au(t,n){var e=n,r=n.L,i=e.U;i?i.L===e?i.L=r:i.R=r:t._=r,r.U=i,e.U=r,e.L=r.R,e.L&&(e.L.U=e),r.R=e}function Eu(t){for(;t.L;)t=t.L;return t}function Cu(t,n,e,r){var i=[null,null],o=om.push(i)-1;return i.left=t,i.right=n,e&&Pu(i,t,n,e),r&&Pu(i,n,t,r),rm[t.index].halfedges.push(o),rm[n.index].halfedges.push(o),i}function zu(t,n,e){var r=[n,e];return r.left=t,r}function Pu(t,n,e,r){t[0]||t[1]?t.left===e?t[1]=r:t[0]=r:(t[0]=r,t.left=n,t.right=e)}function qu(t,n,e,r,i){var o,u=t[0],a=t[1],c=u[0],s=u[1],f=a[0],l=a[1],h=0,p=1,d=f-c,v=l-s;if(o=n-c,d||!(o>0)){if(o/=d,d<0){if(o<h)return;o<p&&(p=o)}else if(d>0){if(o>p)return;o>h&&(h=o)}if(o=r-c,d||!(o<0)){if(o/=d,d<0){if(o>p)return;o>h&&(h=o)}else if(d>0){if(o<h)return;o<p&&(p=o)}if(o=e-s,v||!(o>0)){if(o/=v,v<0){if(o<h)return;o<p&&(p=o)}else if(v>0){if(o>p)return;o>h&&(h=o)}if(o=i-s,v||!(o<0)){if(o/=v,v<0){if(o>p)return;o>h&&(h=o)}else if(v>0){if(o<h)return;o<p&&(p=o)}return!(h>0||p<1)||(h>0&&(t[0]=[c+h*d,s+h*v]),p<1&&(t[1]=[c+p*d,s+p*v]),!0)}}}}}function Lu(t,n,e,r,i){var o=t[1];if(o)return!0;var u,a,c=t[0],s=t.left,f=t.right,l=s[0],h=s[1],p=f[0],d=f[1],v=(l+p)/2,_=(h+d)/2;if(d===h){if(v<n||v>=r)return;if(l>p){if(c){if(c[1]>=i)return}else c=[v,e];o=[v,i]}else{if(c){if(c[1]<e)return}else c=[v,i];o=[v,e]}}else if(u=(l-p)/(d-h),a=_-u*v,u<-1||u>1)if(l>p){if(c){if(c[1]>=i)return}else c=[(e-a)/u,e];o=[(i-a)/u,i]}else{if(c){if(c[1]<e)return}else c=[(i-a)/u,i];o=[(e-a)/u,e]}else if(h<d){if(c){if(c[0]>=r)return}else c=[n,u*n+a];o=[r,u*r+a]}else{if(c){if(c[0]<n)return}else c=[r,u*r+a];o=[n,u*n+a]}return t[0]=c,t[1]=o,!0}function Ru(t,n,e,r){for(var i,o=om.length;o--;)Lu(i=om[o],t,n,e,r)&&qu(i,t,n,e,r)&&(Math.abs(i[0][0]-i[1][0])>cm||Math.abs(i[0][1]-i[1][1])>cm)||delete om[o]}function Uu(t){return rm[t.index]={site:t,halfedges:[]}}function Du(t,n){var e=t.site,r=n.left,i=n.right;return e===i&&(i=r,r=e),i?Math.atan2(i[1]-r[1],i[0]-r[0]):(e===r?(r=n[1],i=n[0]):(r=n[0],i=n[1]),Math.atan2(r[0]-i[0],i[1]-r[1]))}function Ou(t,n){return n[+(n.left!==t.site)]}function Fu(t,n){return n[+(n.left===t.site)]}function Iu(){for(var t,n,e,r,i=0,o=rm.length;i<o;++i)if((t=rm[i])&&(r=(n=t.halfedges).length)){var u=new Array(r),a=new Array(r);for(e=0;e<r;++e)u[e]=e,a[e]=Du(t,om[n[e]]);for(u.sort(function(t,n){return a[n]-a[t]}),e=0;e<r;++e)a[e]=n[u[e]];for(e=0;e<r;++e)n[e]=a[e]}}function Yu(t,n,e,r){var i,o,u,a,c,s,f,l,h,p,d,v,_=rm.length,y=!0;for(i=0;i<_;++i)if(o=rm[i]){for(u=o.site,c=o.halfedges,a=c.length;a--;)om[c[a]]||c.splice(a,1);for(a=0,s=c.length;a<s;)p=Fu(o,om[c[a]]),d=p[0],v=p[1],f=Ou(o,om[c[++a%s]]),l=f[0],h=f[1],(Math.abs(d-l)>cm||Math.abs(v-h)>cm)&&(c.splice(a,0,om.push(zu(u,p,Math.abs(d-t)<cm&&r-v>cm?[t,Math.abs(l-t)<cm?h:r]:Math.abs(v-r)<cm&&e-d>cm?[Math.abs(h-r)<cm?l:e,r]:Math.abs(d-e)<cm&&v-n>cm?[e,Math.abs(l-e)<cm?h:n]:Math.abs(v-n)<cm&&d-t>cm?[Math.abs(h-n)<cm?l:t,n]:null))-1),++s);s&&(y=!1)}if(y){var g,m,x,b=1/0;for(i=0,y=null;i<_;++i)(o=rm[i])&&(u=o.site,g=u[0]-t,m=u[1]-n,x=g*g+m*m,x<b&&(b=x,y=o));if(y){var w=[t,n],M=[t,r],T=[e,r],N=[e,n];y.halfedges.push(om.push(zu(u=y.site,w,M))-1,om.push(zu(u,M,T))-1,om.push(zu(u,T,N))-1,om.push(zu(u,N,w))-1)}}for(i=0;i<_;++i)(o=rm[i])&&(o.halfedges.length||delete rm[i])}function Bu(){ku(this),this.x=this.y=this.arc=this.site=this.cy=null}function ju(t){var n=t.P,e=t.N;if(n&&e){var r=n.site,i=t.site,o=e.site;if(r!==o){var u=i[0],a=i[1],c=r[0]-u,s=r[1]-a,f=o[0]-u,l=o[1]-a,h=2*(c*l-s*f);if(!(h>=-sm)){var p=c*c+s*s,d=f*f+l*l,v=(l*p-s*d)/h,_=(c*d-f*p)/h,y=um.pop()||new Bu;y.arc=t,y.site=i,y.x=v+u,y.y=(y.cy=_+a)+Math.sqrt(v*v+_*_),t.circle=y;for(var g=null,m=im._;m;)if(y.y<m.y||y.y===m.y&&y.x<=m.x){if(!m.L){g=m.P;break}m=m.L}else{if(!m.R){g=m;break}m=m.R}im.insert(g,y),g||(nm=y)}}}}function Hu(t){var n=t.circle;n&&(n.P||(nm=n.N),im.remove(n),um.push(n),ku(n),t.circle=null)}function Xu(){ku(this),this.edge=this.site=this.circle=null}function Vu(t){var n=am.pop()||new Xu;return n.site=t,n}function Wu(t){Hu(t),em.remove(t),am.push(t),ku(t)}function $u(t){var n=t.circle,e=n.x,r=n.cy,i=[e,r],o=t.P,u=t.N,a=[t];Wu(t);for(var c=o;c.circle&&Math.abs(e-c.circle.x)<cm&&Math.abs(r-c.circle.cy)<cm;)o=c.P,a.unshift(c),Wu(c),c=o;a.unshift(c),Hu(c);for(var s=u;s.circle&&Math.abs(e-s.circle.x)<cm&&Math.abs(r-s.circle.cy)<cm;)u=s.N,a.push(s),Wu(s),s=u;a.push(s),Hu(s);var f,l=a.length;for(f=1;f<l;++f)s=a[f],c=a[f-1],Pu(s.edge,c.site,s.site,i);c=a[0],s=a[l-1],s.edge=Cu(c.site,s.site,null,i),ju(c),ju(s)}function Zu(t){for(var n,e,r,i,o=t[0],u=t[1],a=em._;a;)if(r=Gu(a,u)-o,r>cm)a=a.L;else{if(i=o-Ju(a,u),!(i>cm)){r>-cm?(n=a.P,e=a):i>-cm?(n=a,e=a.N):n=e=a;break}if(!a.R){n=a;break}a=a.R}Uu(t);var c=Vu(t);if(em.insert(n,c),n||e){if(n===e)return Hu(n),e=Vu(n.site),em.insert(c,e),c.edge=e.edge=Cu(n.site,c.site),ju(n),void ju(e);if(!e)return void(c.edge=Cu(n.site,c.site));Hu(n),Hu(e);var s=n.site,f=s[0],l=s[1],h=t[0]-f,p=t[1]-l,d=e.site,v=d[0]-f,_=d[1]-l,y=2*(h*_-p*v),g=h*h+p*p,m=v*v+_*_,x=[(_*g-p*m)/y+f,(h*m-v*g)/y+l];Pu(e.edge,s,d,x),c.edge=Cu(s,t,null,x),e.edge=Cu(t,d,null,x),ju(n),ju(e)}}function Gu(t,n){var e=t.site,r=e[0],i=e[1],o=i-n;if(!o)return r;var u=t.P;if(!u)return-(1/0);e=u.site;var a=e[0],c=e[1],s=c-n;if(!s)return a;var f=a-r,l=1/o-1/s,h=f/s;return l?(-h+Math.sqrt(h*h-2*l*(f*f/(-2*s)-c+s/2+i-o/2)))/l+r:(r+a)/2}function Ju(t,n){var e=t.N;if(e)return Gu(e,n);var r=t.site;return r[1]===n?r[0]:1/0}function Qu(t,n,e){return(t[0]-e[0])*(n[1]-t[1])-(t[0]-n[0])*(e[1]-t[1])}function Ku(t,n){return n[1]-t[1]||n[0]-t[0]}function ta(t,n){var e,r,i,o=t.sort(Ku).pop();for(om=[],rm=new Array(t.length),em=new Nu,im=new Nu;;)if(i=nm,o&&(!i||o[1]<i.y||o[1]===i.y&&o[0]<i.x))o[0]===e&&o[1]===r||(Zu(o),e=o[0],r=o[1]),o=t.pop();else{if(!i)break;$u(i.arc)}if(Iu(),n){var u=+n[0][0],a=+n[0][1],c=+n[1][0],s=+n[1][1];Ru(u,a,c,s),Yu(u,a,c,s)}this.edges=om,this.cells=rm,em=im=om=rm=null}function na(t,n,e){this.target=t,this.type=n,this.transform=e}function ea(t,n,e){this.k=t,this.x=n,this.y=e}function ra(t){return t.__zoom||hm}function ia(){t.event.stopImmediatePropagation()}function oa(){return!t.event.button}function ua(){var t,n,e=this;return e instanceof SVGElement?(e=e.ownerSVGElement||e,t=e.width.baseVal.value,n=e.height.baseVal.value):(t=e.clientWidth,n=e.clientHeight),[[0,0],[t,n]]}function aa(){return this.__zoom||hm}function ca(){t.event.stopImmediatePropagation()}function sa(t){return{type:t}}function fa(){return!t.event.button}function la(){var t=this.ownerSVGElement||this;return[[0,0],[t.width.baseVal.value,t.height.baseVal.value]]}function ha(t){for(;!t.__brush;)if(!(t=t.parentNode))return;return t.__brush}function pa(t){return t[0][0]===t[1][0]||t[0][1]===t[1][1]}function da(t){var n=t.__brush;return n?n.dim.output(n.selection):null}function va(){return ya(wm)}function _a(){return ya(Mm)}function ya(n){function e(t){var e=t.property("__brush",a).selectAll(".overlay").data([sa("overlay")]);e.enter().append("rect").attr("class","overlay").attr("pointer-events","all").attr("cursor",Nm.overlay).merge(e).each(function(){var t=ha(this).extent;dy(this).attr("x",t[0][0]).attr("y",t[0][1]).attr("width",t[1][0]-t[0][0]).attr("height",t[1][1]-t[0][1])}),t.selectAll(".selection").data([sa("selection")]).enter().append("rect").attr("class","selection").attr("cursor",Nm.selection).attr("fill","#777").attr("fill-opacity",.3).attr("stroke","#fff").attr("shape-rendering","crispEdges");var i=t.selectAll(".handle").data(n.handles,function(t){return t.type});i.exit().remove(),i.enter().append("rect").attr("class",function(t){return"handle handle--"+t.type}).attr("cursor",function(t){return Nm[t.type]}),t.each(r).attr("fill","none").attr("pointer-events","all").style("-webkit-tap-highlight-color","rgba(0,0,0,0)").on("mousedown.brush touchstart.brush",u)}function r(){var t=dy(this),n=ha(this).selection;n?(t.selectAll(".selection").style("display",null).attr("x",n[0][0]).attr("y",n[0][1]).attr("width",n[1][0]-n[0][0]).attr("height",n[1][1]-n[0][1]),t.selectAll(".handle").style("display",null).attr("x",function(t){return"e"===t.type[t.type.length-1]?n[1][0]-h/2:n[0][0]-h/2}).attr("y",function(t){return"s"===t.type[0]?n[1][1]-h/2:n[0][1]-h/2}).attr("width",function(t){return"n"===t.type||"s"===t.type?n[1][0]-n[0][0]+h:h}).attr("height",function(t){return"e"===t.type||"w"===t.type?n[1][1]-n[0][1]+h:h})):t.selectAll(".selection,.handle").style("display","none").attr("x",null).attr("y",null).attr("width",null).attr("height",null)}function i(t,n){return t.__brush.emitter||new o(t,n)}function o(t,n){this.that=t,this.args=n,this.state=t.__brush,this.active=0}function u(){function e(){var t=P_(T);!U||w||M||(Math.abs(t[0]-O[0])>Math.abs(t[1]-O[1])?M=!0:w=!0),O=t,b=!0,ym(),o()}function o(){var t;switch(m=O[0]-D[0],x=O[1]-D[1],k){case mm:case gm:S&&(m=Math.max(P-l,Math.min(L-v,m)),h=l+m,_=v+m),A&&(x=Math.max(q-p,Math.min(R-y,x)),d=p+x,g=y+x);break;case xm:S<0?(m=Math.max(P-l,Math.min(L-l,m)),h=l+m,_=v):S>0&&(m=Math.max(P-v,Math.min(L-v,m)),h=l,_=v+m),A<0?(x=Math.max(q-p,Math.min(R-p,x)),d=p+x,g=y):A>0&&(x=Math.max(q-y,Math.min(R-y,x)),d=p,g=y+x);break;case bm:S&&(h=Math.max(P,Math.min(L,l-m*S)),_=Math.max(P,Math.min(L,v+m*S))),A&&(d=Math.max(q,Math.min(R,p-x*A)),g=Math.max(q,Math.min(R,y+x*A)))}_<h&&(S*=-1,t=l,l=v,v=t,t=h,h=_,_=t,N in km&&Y.attr("cursor",Nm[N=km[N]])),g<d&&(A*=-1,t=p,p=y,y=t,t=d,d=g,g=t,N in Sm&&Y.attr("cursor",Nm[N=Sm[N]])),E.selection&&(z=E.selection),w&&(h=z[0][0],_=z[1][0]),M&&(d=z[0][1],g=z[1][1]),z[0][0]===h&&z[0][1]===d&&z[1][0]===_&&z[1][1]===g||(E.selection=[[h,d],[_,g]],r.call(T),F.brush())}function u(){if(ca(),t.event.touches){if(t.event.touches.length)return;c&&clearTimeout(c),c=setTimeout(function(){c=null},500),I.on("touchmove.brush touchend.brush touchcancel.brush",null)}else gu(t.event.view,b),B.on("keydown.brush keyup.brush mousemove.brush mouseup.brush",null);I.attr("pointer-events","all"),Y.attr("cursor",Nm.overlay),E.selection&&(z=E.selection),pa(z)&&(E.selection=null,r.call(T)),F.end()}function a(){switch(t.event.keyCode){case 16:U=S&&A;break;case 18:k===xm&&(S&&(v=_-m*S,l=h+m*S),A&&(y=g-x*A,p=d+x*A),k=bm,o());break;case 32:k!==xm&&k!==bm||(S<0?v=_-m:S>0&&(l=h-m),A<0?y=g-x:A>0&&(p=d-x),k=mm,Y.attr("cursor",Nm.selection),o());break;default:return}ym()}function s(){switch(t.event.keyCode){case 16:U&&(w=M=U=!1,o());break;case 18:k===bm&&(S<0?v=_:S>0&&(l=h),A<0?y=g:A>0&&(p=d),k=xm,o());break;case 32:k===mm&&(t.event.altKey?(S&&(v=_-m*S,l=h+m*S),A&&(y=g-x*A,p=d+x*A),k=bm):(S<0?v=_:S>0&&(l=h),A<0?y=g:A>0&&(p=d),k=xm),Y.attr("cursor",Nm[N]),o());break;default:return}ym()}if(t.event.touches){if(t.event.changedTouches.length<t.event.touches.length)return ym()}else if(c)return;if(f.apply(this,arguments)){var l,h,p,d,v,_,y,g,m,x,b,w,M,T=this,N=t.event.target.__data__.type,k="selection"===(t.event.metaKey?N="overlay":N)?gm:t.event.altKey?bm:xm,S=n===Mm?null:Am[N],A=n===wm?null:Em[N],E=ha(T),C=E.extent,z=E.selection,P=C[0][0],q=C[0][1],L=C[1][0],R=C[1][1],U=S&&A&&t.event.shiftKey,D=P_(T),O=D,F=i(T,arguments).beforestart();"overlay"===N?E.selection=z=[[l=n===Mm?P:D[0],p=n===wm?q:D[1]],[v=n===Mm?L:l,y=n===wm?R:p]]:(l=z[0][0],p=z[0][1],v=z[1][0],y=z[1][1]),h=l,d=p,_=v,g=y;var I=dy(T).attr("pointer-events","none"),Y=I.selectAll(".overlay").attr("cursor",Nm[N]);if(t.event.touches)I.on("touchmove.brush",e,!0).on("touchend.brush touchcancel.brush",u,!0);else{var B=dy(t.event.view).on("keydown.brush",a,!0).on("keyup.brush",s,!0).on("mousemove.brush",e,!0).on("mouseup.brush",u,!0);Jg(t.event.view)}ca(),Ay(T),r.call(T),F.start()}}function a(){var t=this.__brush||{selection:null};return t.extent=s.apply(this,arguments),t.dim=n,t}var c,s=la,f=fa,l=Pn(e,"start","brush","end"),h=6;return e.move=function(t,e){t.selection?t.on("start.brush",function(){i(this,arguments).beforestart().start()}).on("interrupt.brush end.brush",function(){i(this,arguments).end()}).tween("brush",function(){function t(t){u.selection=1===t&&pa(s)?null:f(t),r.call(o),a.brush()}var o=this,u=o.__brush,a=i(o,arguments),c=u.selection,s=n.input("function"==typeof e?e.apply(this,arguments):e,u.extent),f=hp(c,s);return c&&s?t:t(1);
}):t.each(function(){var t=this,o=arguments,u=t.__brush,a=n.input("function"==typeof e?e.apply(t,o):e,u.extent),c=i(t,o).beforestart();Ay(t),u.selection=null==a||pa(a)?null:a,r.call(t),c.start().brush().end()})},o.prototype={beforestart:function(){return 1===++this.active&&(this.state.emitter=this,this.starting=!0),this},start:function(){return this.starting&&(this.starting=!1,this.emit("start")),this},brush:function(){return this.emit("brush"),this},end:function(){return 0===--this.active&&(delete this.state.emitter,this.emit("end")),this},emit:function(t){Xr(new _m(e,t,n.output(this.state.selection)),l.apply,l,[t,this.that,this.args])}},e.extent=function(t){return arguments.length?(s="function"==typeof t?t:vm([[+t[0][0],+t[0][1]],[+t[1][0],+t[1][1]]]),e):s},e.filter=function(t){return arguments.length?(f="function"==typeof t?t:vm(!!t),e):f},e.handleSize=function(t){return arguments.length?(h=+t,e):h},e.on=function(){var t=l.on.apply(l,arguments);return t===l?e:t},e}function ga(t){return function(n,e){return t(n.source.value+n.target.value,e.source.value+e.target.value)}}function ma(t){return t.source}function xa(t){return t.target}function ba(t){return t.radius}function wa(t){return t.startAngle}function Ma(t){return t.endAngle}function Ta(){this.reset()}function Na(t,n,e){var r=t.s=n+e,i=r-n,o=r-i;t.t=n-o+(e-i)}function ka(t){return t>1?0:t<-1?wx:Math.acos(t)}function Sa(t){return t>1?Mx:t<-1?-Mx:Math.asin(t)}function Aa(t){return(t=Ux(t/2))*t}function Ea(){}function Ca(t,n){t&&Yx.hasOwnProperty(t.type)&&Yx[t.type](t,n)}function za(t,n,e){var r,i=-1,o=t.length-e;for(n.lineStart();++i<o;)r=t[i],n.point(r[0],r[1],r[2]);n.lineEnd()}function Pa(t,n){var e=-1,r=t.length;for(n.polygonStart();++e<r;)za(t[e],n,1);n.polygonEnd()}function qa(){Xx.point=Ra}function La(){Ua(Bm,jm)}function Ra(t,n){Xx.point=Ua,Bm=t,jm=n,t*=Sx,n*=Sx,Hm=t,Xm=zx(n=n/2+Tx),Vm=Ux(n)}function Ua(t,n){t*=Sx,n*=Sx,n=n/2+Tx;var e=t-Hm,r=e>=0?1:-1,i=r*e,o=zx(n),u=Ux(n),a=Vm*u,c=Xm*o+a*zx(i),s=a*r*Ux(i);jx.add(Cx(s,c)),Hm=t,Xm=o,Vm=u}function Da(t){return[Cx(t[1],t[0]),Sa(t[2])]}function Oa(t){var n=t[0],e=t[1],r=zx(e);return[r*zx(n),r*Ux(n),Ux(e)]}function Fa(t,n){return t[0]*n[0]+t[1]*n[1]+t[2]*n[2]}function Ia(t,n){return[t[1]*n[2]-t[2]*n[1],t[2]*n[0]-t[0]*n[2],t[0]*n[1]-t[1]*n[0]]}function Ya(t,n){t[0]+=n[0],t[1]+=n[1],t[2]+=n[2]}function Ba(t,n){return[t[0]*n,t[1]*n,t[2]*n]}function ja(t){var n=Ox(t[0]*t[0]+t[1]*t[1]+t[2]*t[2]);t[0]/=n,t[1]/=n,t[2]/=n}function Ha(t,n){nx.push(ex=[Wm=t,Zm=t]),n<$m&&($m=n),n>Gm&&(Gm=n)}function Xa(t,n){var e=Oa([t*Sx,n*Sx]);if(tx){var r=Ia(tx,e),i=[r[1],-r[0],0],o=Ia(i,r);ja(o),o=Da(o);var u,a=t-Jm,c=a>0?1:-1,s=o[0]*kx*c,f=Ax(a)>180;f^(c*Jm<s&&s<c*t)?(u=o[1]*kx,u>Gm&&(Gm=u)):(s=(s+360)%360-180,f^(c*Jm<s&&s<c*t)?(u=-o[1]*kx,u<$m&&($m=u)):(n<$m&&($m=n),n>Gm&&(Gm=n))),f?t<Jm?Ja(Wm,t)>Ja(Wm,Zm)&&(Zm=t):Ja(t,Zm)>Ja(Wm,Zm)&&(Wm=t):Zm>=Wm?(t<Wm&&(Wm=t),t>Zm&&(Zm=t)):t>Jm?Ja(Wm,t)>Ja(Wm,Zm)&&(Zm=t):Ja(t,Zm)>Ja(Wm,Zm)&&(Wm=t)}else Ha(t,n);tx=e,Jm=t}function Va(){$x.point=Xa}function Wa(){ex[0]=Wm,ex[1]=Zm,$x.point=Ha,tx=null}function $a(t,n){if(tx){var e=t-Jm;Wx.add(Ax(e)>180?e+(e>0?360:-360):e)}else Qm=t,Km=n;Xx.point(t,n),Xa(t,n)}function Za(){Xx.lineStart()}function Ga(){$a(Qm,Km),Xx.lineEnd(),Ax(Wx)>xx&&(Wm=-(Zm=180)),ex[0]=Wm,ex[1]=Zm,tx=null}function Ja(t,n){return(n-=t)<0?n+360:n}function Qa(t,n){return t[0]-n[0]}function Ka(t,n){return t[0]<=t[1]?t[0]<=n&&n<=t[1]:n<t[0]||t[1]<n}function tc(t,n){t*=Sx,n*=Sx;var e=zx(n);nc(e*zx(t),e*Ux(t),Ux(n))}function nc(t,n,e){++rx,ox+=(t-ox)/rx,ux+=(n-ux)/rx,ax+=(e-ax)/rx}function ec(){Gx.point=rc}function rc(t,n){t*=Sx,n*=Sx;var e=zx(n);_x=e*zx(t),yx=e*Ux(t),gx=Ux(n),Gx.point=ic,nc(_x,yx,gx)}function ic(t,n){t*=Sx,n*=Sx;var e=zx(n),r=e*zx(t),i=e*Ux(t),o=Ux(n),u=Cx(Ox((u=yx*o-gx*i)*u+(u=gx*r-_x*o)*u+(u=_x*i-yx*r)*u),_x*r+yx*i+gx*o);ix+=u,cx+=u*(_x+(_x=r)),sx+=u*(yx+(yx=i)),fx+=u*(gx+(gx=o)),nc(_x,yx,gx)}function oc(){Gx.point=tc}function uc(){Gx.point=cc}function ac(){sc(dx,vx),Gx.point=tc}function cc(t,n){dx=t,vx=n,t*=Sx,n*=Sx,Gx.point=sc;var e=zx(n);_x=e*zx(t),yx=e*Ux(t),gx=Ux(n),nc(_x,yx,gx)}function sc(t,n){t*=Sx,n*=Sx;var e=zx(n),r=e*zx(t),i=e*Ux(t),o=Ux(n),u=yx*o-gx*i,a=gx*r-_x*o,c=_x*i-yx*r,s=Ox(u*u+a*a+c*c),f=_x*r+yx*i+gx*o,l=s&&-ka(f)/s,h=Cx(s,f);lx+=l*u,hx+=l*a,px+=l*c,ix+=h,cx+=h*(_x+(_x=r)),sx+=h*(yx+(yx=i)),fx+=h*(gx+(gx=o)),nc(_x,yx,gx)}function fc(t,n){return[t>wx?t-Nx:t<-wx?t+Nx:t,n]}function lc(t,n,e){return(t%=Nx)?n||e?Kx(pc(t),dc(n,e)):pc(t):n||e?dc(n,e):fc}function hc(t){return function(n,e){return n+=t,[n>wx?n-Nx:n<-wx?n+Nx:n,e]}}function pc(t){var n=hc(t);return n.invert=hc(-t),n}function dc(t,n){function e(t,n){var e=zx(n),a=zx(t)*e,c=Ux(t)*e,s=Ux(n),f=s*r+a*i;return[Cx(c*o-f*u,a*r-s*i),Sa(f*o+c*u)]}var r=zx(t),i=Ux(t),o=zx(n),u=Ux(n);return e.invert=function(t,n){var e=zx(n),a=zx(t)*e,c=Ux(t)*e,s=Ux(n),f=s*o-c*u;return[Cx(c*o+s*u,a*r+f*i),Sa(f*r-a*i)]},e}function vc(t,n,e,r,i,o){if(e){var u=zx(n),a=Ux(n),c=r*e;null==i?(i=n+r*Nx,o=n-c/2):(i=_c(u,i),o=_c(u,o),(r>0?i<o:i>o)&&(i+=r*Nx));for(var s,f=i;r>0?f>o:f<o;f-=c)s=Da([u,-a*zx(f),-a*Ux(f)]),t.point(s[0],s[1])}}function _c(t,n){n=Oa(n),n[0]-=t,ja(n);var e=ka(-n[1]);return((-n[2]<0?-e:e)+Nx-xx)%Nx}function yc(t,n,e,r){this.x=t,this.z=n,this.o=e,this.e=r,this.v=!1,this.n=this.p=null}function gc(t){if(n=t.length){for(var n,e,r=0,i=t[0];++r<n;)i.n=e=t[r],e.p=i,i=e;i.n=e=t[0],e.p=i}}function mc(t,n,e,r){function i(i,o){return t<=i&&i<=e&&n<=o&&o<=r}function o(i,o,a,s){var f=0,l=0;if(null==i||(f=u(i,a))!==(l=u(o,a))||c(i,o)<0^a>0){do s.point(0===f||3===f?t:e,f>1?r:n);while((f=(f+a+4)%4)!==l)}else s.point(o[0],o[1])}function u(r,i){return Ax(r[0]-t)<xx?i>0?0:3:Ax(r[0]-e)<xx?i>0?2:1:Ax(r[1]-n)<xx?i>0?1:0:i>0?3:2}function a(t,n){return c(t.x,n.x)}function c(t,n){var e=u(t,1),r=u(n,1);return e!==r?e-r:0===e?n[1]-t[1]:1===e?t[0]-n[0]:2===e?t[1]-n[1]:n[0]-t[0]}return function(u){function c(t,n){i(t,n)&&k.point(t,n)}function s(){for(var n=0,e=0,i=_.length;e<i;++e)for(var o,u,a=_[e],c=1,s=a.length,f=a[0],l=f[0],h=f[1];c<s;++c)o=l,u=h,f=a[c],l=f[0],h=f[1],u<=r?h>r&&(l-o)*(r-u)>(h-u)*(t-o)&&++n:h<=r&&(l-o)*(r-u)<(h-u)*(t-o)&&--n;return n}function f(){k=S,v=[],_=[],N=!0}function l(){var t=s(),n=N&&t,e=(v=Ws(v)).length;(n||e)&&(u.polygonStart(),n&&(u.lineStart(),o(null,null,1,u),u.lineEnd()),e&&_b(v,a,t,o,u),u.polygonEnd()),k=u,v=_=y=null}function h(){A.point=d,_&&_.push(y=[]),T=!0,M=!1,b=w=NaN}function p(){v&&(d(g,m),x&&M&&S.rejoin(),v.push(S.result())),A.point=c,M&&k.lineEnd()}function d(o,u){var a=i(o,u);if(_&&y.push([o,u]),T)g=o,m=u,x=a,T=!1,a&&(k.lineStart(),k.point(o,u));else if(a&&M)k.point(o,u);else{var c=[b=Math.max(gb,Math.min(yb,b)),w=Math.max(gb,Math.min(yb,w))],s=[o=Math.max(gb,Math.min(yb,o)),u=Math.max(gb,Math.min(yb,u))];db(c,s,t,n,e,r)?(M||(k.lineStart(),k.point(c[0],c[1])),k.point(s[0],s[1]),a||k.lineEnd(),N=!1):a&&(k.lineStart(),k.point(o,u),N=!1)}b=o,w=u,M=a}var v,_,y,g,m,x,b,w,M,T,N,k=u,S=pb(),A={point:c,lineStart:h,lineEnd:p,polygonStart:f,polygonEnd:l};return A}}function xc(){bb.point=wc,bb.lineEnd=bc}function bc(){bb.point=bb.lineEnd=Ea}function wc(t,n){t*=Sx,n*=Sx,tb=t,nb=Ux(n),eb=zx(n),bb.point=Mc}function Mc(t,n){t*=Sx,n*=Sx;var e=Ux(n),r=zx(n),i=Ax(t-tb),o=zx(i),u=Ux(i),a=r*u,c=eb*e-nb*r*o,s=nb*e+eb*r*o;xb.add(Cx(Ox(a*a+c*c),s)),tb=t,nb=e,eb=r}function Tc(t,n,e){var r=Ls(t,n-xx,e).concat(n);return function(t){return r.map(function(n){return[t,n]})}}function Nc(t,n,e){var r=Ls(t,n-xx,e).concat(n);return function(t){return r.map(function(n){return[n,t]})}}function kc(){zb.point=Sc}function Sc(t,n){zb.point=Ac,rb=ob=t,ib=ub=n}function Ac(t,n){Cb.add(ub*t-ob*n),ob=t,ub=n}function Ec(){Ac(rb,ib)}function Cc(t,n){t<Pb&&(Pb=t),t>Lb&&(Lb=t),n<qb&&(qb=n),n>Rb&&(Rb=n)}function zc(t,n){Db+=t,Ob+=n,++Fb}function Pc(){Vb.point=qc}function qc(t,n){Vb.point=Lc,zc(sb=t,fb=n)}function Lc(t,n){var e=t-sb,r=n-fb,i=Ox(e*e+r*r);Ib+=i*(sb+t)/2,Yb+=i*(fb+n)/2,Bb+=i,zc(sb=t,fb=n)}function Rc(){Vb.point=zc}function Uc(){Vb.point=Oc}function Dc(){Fc(ab,cb)}function Oc(t,n){Vb.point=Fc,zc(ab=sb=t,cb=fb=n)}function Fc(t,n){var e=t-sb,r=n-fb,i=Ox(e*e+r*r);Ib+=i*(sb+t)/2,Yb+=i*(fb+n)/2,Bb+=i,i=fb*t-sb*n,jb+=i*(sb+t),Hb+=i*(fb+n),Xb+=3*i,zc(sb=t,fb=n)}function Ic(t){this._context=t}function Yc(){this._string=[]}function Bc(t){return"m0,"+t+"a"+t+","+t+" 0 1,1 0,"+-2*t+"a"+t+","+t+" 0 1,1 0,"+2*t+"z"}function jc(t){return t.length>1}function Hc(t,n){return((t=t.x)[0]<0?t[1]-Mx-xx:Mx-t[1])-((n=n.x)[0]<0?n[1]-Mx-xx:Mx-n[1])}function Xc(t){var n,e=NaN,r=NaN,i=NaN;return{lineStart:function(){t.lineStart(),n=1},point:function(o,u){var a=o>0?wx:-wx,c=Ax(o-e);Ax(c-wx)<xx?(t.point(e,r=(r+u)/2>0?Mx:-Mx),t.point(i,r),t.lineEnd(),t.lineStart(),t.point(a,r),t.point(o,r),n=0):i!==a&&c>=wx&&(Ax(e-i)<xx&&(e-=i*xx),Ax(o-a)<xx&&(o-=a*xx),r=Vc(e,r,o,u),t.point(i,r),t.lineEnd(),t.lineStart(),t.point(a,r),n=0),t.point(e=o,r=u),i=a},lineEnd:function(){t.lineEnd(),e=r=NaN},clean:function(){return 2-n}}}function Vc(t,n,e,r){var i,o,u=Ux(t-e);return Ax(u)>xx?Ex((Ux(n)*(o=zx(r))*Ux(e)-Ux(r)*(i=zx(n))*Ux(t))/(i*o*u)):(n+r)/2}function Wc(t,n,e,r){var i;if(null==t)i=e*Mx,r.point(-wx,i),r.point(0,i),r.point(wx,i),r.point(wx,0),r.point(wx,-i),r.point(0,-i),r.point(-wx,-i),r.point(-wx,0),r.point(-wx,i);else if(Ax(t[0]-n[0])>xx){var o=t[0]<n[0]?wx:-wx;i=e*o/2,r.point(-o,i),r.point(0,i),r.point(o,i)}else r.point(n[0],n[1])}function $c(t){function n(){}var e=n.prototype=Object.create(Zc.prototype);for(var r in t)e[r]=t[r];return function(t){var e=new n;return e.stream=t,e}}function Zc(){}function Gc(t,n,e){var r=n[1][0]-n[0][0],i=n[1][1]-n[0][1],o=t.clipExtent&&t.clipExtent();t.scale(150).translate([0,0]),null!=o&&t.clipExtent(null),Bx(e,t.stream(Ub));var u=Ub.result(),a=Math.min(r/(u[1][0]-u[0][0]),i/(u[1][1]-u[0][1])),c=+n[0][0]+(r-a*(u[1][0]+u[0][0]))/2,s=+n[0][1]+(i-a*(u[1][1]+u[0][1]))/2;return null!=o&&t.clipExtent(o),t.scale(150*a).translate([c,s])}function Jc(t){return function(n,e){return Gc(t,[[0,0],n],e)}}function Qc(t){return function(n,e){return Gc(t,n,e)}}function Kc(t){return $c({point:function(n,e){n=t(n,e),this.stream.point(n[0],n[1])}})}function ts(t,n){function e(r,i,o,u,a,c,s,f,l,h,p,d,v,_){var y=s-r,g=f-i,m=y*y+g*g;if(m>4*n&&v--){var x=u+h,b=a+p,w=c+d,M=Ox(x*x+b*b+w*w),T=Sa(w/=M),N=Ax(Ax(w)-1)<xx||Ax(o-l)<xx?(o+l)/2:Cx(b,x),k=t(N,T),S=k[0],A=k[1],E=S-r,C=A-i,z=g*E-y*C;(z*z/m>n||Ax((y*E+g*C)/m-.5)>.3||u*h+a*p+c*d<nw)&&(e(r,i,o,u,a,c,S,A,N,x/=M,b/=M,w,v,_),_.point(S,A),e(S,A,N,x,b,w,s,f,l,h,p,d,v,_))}}return function(n){function r(e,r){e=t(e,r),n.point(e[0],e[1])}function i(){y=NaN,w.point=o,n.lineStart()}function o(r,i){var o=Oa([r,i]),u=t(r,i);e(y,g,_,m,x,b,y=u[0],g=u[1],_=r,m=o[0],x=o[1],b=o[2],tw,n),n.point(y,g)}function u(){w.point=r,n.lineEnd()}function a(){i(),w.point=c,w.lineEnd=s}function c(t,n){o(f=t,n),l=y,h=g,p=m,d=x,v=b,w.point=o}function s(){e(y,g,_,m,x,b,l,h,f,p,d,v,tw,n),w.lineEnd=u,u()}var f,l,h,p,d,v,_,y,g,m,x,b,w={point:r,lineStart:i,lineEnd:u,polygonStart:function(){n.polygonStart(),w.lineStart=a},polygonEnd:function(){n.polygonEnd(),w.lineStart=i}};return w}}function ns(t){return es(function(){return t})()}function es(t){function n(t){return t=f(t[0]*Sx,t[1]*Sx),[t[0]*_+a,c-t[1]*_]}function e(t){return t=f.invert((t[0]-a)/_,(c-t[1])/_),t&&[t[0]*kx,t[1]*kx]}function r(t,n){return t=u(t,n),[t[0]*_+a,c-t[1]*_]}function i(){f=Kx(s=lc(b,w,M),u);var t=u(m,x);return a=y-t[0]*_,c=g+t[1]*_,o()}function o(){return d=v=null,n}var u,a,c,s,f,l,h,p,d,v,_=150,y=480,g=250,m=0,x=0,b=0,w=0,M=0,T=null,N=Jb,k=null,S=Ab,A=.5,E=ew(r,A);return n.stream=function(t){return d&&v===t?d:d=rw(N(s,E(S(v=t))))},n.clipAngle=function(t){return arguments.length?(N=+t?Qb(T=t*Sx,6*Sx):(T=null,Jb),o()):T*kx},n.clipExtent=function(t){return arguments.length?(S=null==t?(k=l=h=p=null,Ab):mc(k=+t[0][0],l=+t[0][1],h=+t[1][0],p=+t[1][1]),o()):null==k?null:[[k,l],[h,p]]},n.scale=function(t){return arguments.length?(_=+t,i()):_},n.translate=function(t){return arguments.length?(y=+t[0],g=+t[1],i()):[y,g]},n.center=function(t){return arguments.length?(m=t[0]%360*Sx,x=t[1]%360*Sx,i()):[m*kx,x*kx]},n.rotate=function(t){return arguments.length?(b=t[0]%360*Sx,w=t[1]%360*Sx,M=t.length>2?t[2]%360*Sx:0,i()):[b*kx,w*kx,M*kx]},n.precision=function(t){return arguments.length?(E=ew(r,A=t*t),o()):Ox(A)},n.fitExtent=Qc(n),n.fitSize=Jc(n),function(){return u=t.apply(this,arguments),n.invert=u.invert&&e,i()}}function rs(t){var n=0,e=wx/3,r=es(t),i=r(n,e);return i.parallels=function(t){return arguments.length?r(n=t[0]*Sx,e=t[1]*Sx):[n*kx,e*kx]},i}function is(t,n){function e(t,n){var e=Ox(o-2*i*Ux(n))/i;return[e*Ux(t*=i),u-e*zx(t)]}var r=Ux(t),i=(r+Ux(n))/2,o=1+r*(2*i-r),u=Ox(o)/i;return e.invert=function(t,n){var e=u-n;return[Cx(t,e)/i,Sa((o-(t*t+e*e)*i*i)/(2*i))]},e}function os(t){var n=t.length;return{point:function(e,r){for(var i=-1;++i<n;)t[i].point(e,r)},sphere:function(){for(var e=-1;++e<n;)t[e].sphere()},lineStart:function(){for(var e=-1;++e<n;)t[e].lineStart()},lineEnd:function(){for(var e=-1;++e<n;)t[e].lineEnd()},polygonStart:function(){for(var e=-1;++e<n;)t[e].polygonStart()},polygonEnd:function(){for(var e=-1;++e<n;)t[e].polygonEnd()}}}function us(t){return function(n,e){var r=zx(n),i=zx(e),o=t(r*i);return[o*i*Ux(n),o*Ux(e)]}}function as(t){return function(n,e){var r=Ox(n*n+e*e),i=t(r),o=Ux(i),u=zx(i);return[Cx(n*o,r*u),Sa(r&&e*o/r)]}}function cs(t,n){return[t,Lx(Fx((Mx+n)/2))]}function ss(t){var n,e=ns(t),r=e.scale,i=e.translate,o=e.clipExtent;return e.scale=function(t){return arguments.length?(r(t),n&&e.clipExtent(null),e):r()},e.translate=function(t){return arguments.length?(i(t),n&&e.clipExtent(null),e):i()},e.clipExtent=function(t){if(!arguments.length)return n?null:o();if(n=null==t){var u=wx*r(),a=i();t=[[a[0]-u,a[1]-u],[a[0]+u,a[1]+u]]}return o(t),e},e.clipExtent(null)}function fs(t){return Fx((Mx+t)/2)}function ls(t,n){function e(t,n){o>0?n<-Mx+xx&&(n=-Mx+xx):n>Mx-xx&&(n=Mx-xx);var e=o/Rx(fs(n),i);return[e*Ux(i*t),o-e*zx(i*t)]}var r=zx(t),i=t===n?Ux(t):Lx(r/zx(n))/Lx(fs(n)/fs(t)),o=r*Rx(fs(t),i)/i;return i?(e.invert=function(t,n){var e=o-n,r=Dx(i)*Ox(t*t+e*e);return[Cx(t,e)/i,2*Ex(Rx(o/r,1/i))-Mx]},e):cs}function hs(t,n){return[t,n]}function ps(t,n){function e(t,n){var e=o-n,r=i*t;return[e*Ux(r),o-e*zx(r)]}var r=zx(t),i=t===n?Ux(t):(r-zx(n))/(n-t),o=r/i+t;return Ax(i)<xx?hs:(e.invert=function(t,n){var e=o-n;return[Cx(t,e)/i,o-Dx(i)*Ox(t*t+e*e)]},e)}function ds(t,n){var e=zx(n),r=zx(t)*e;return[e*Ux(t)/r,Ux(n)/r]}function vs(t,n){return[zx(n)*Ux(t),Ux(n)]}function _s(t,n){var e=zx(n),r=1+zx(t)*e;return[e*Ux(t)/r,Ux(n)/r]}function ys(t,n){return[Lx(Fx((Mx+n)/2)),-t]}var gs="4.2.6",ms=function(t,n){return t<n?-1:t>n?1:t>=n?0:NaN},xs=function(t){return 1===t.length&&(t=n(t)),{left:function(n,e,r,i){for(null==r&&(r=0),null==i&&(i=n.length);r<i;){var o=r+i>>>1;t(n[o],e)<0?r=o+1:i=o}return r},right:function(n,e,r,i){for(null==r&&(r=0),null==i&&(i=n.length);r<i;){var o=r+i>>>1;t(n[o],e)>0?i=o:r=o+1}return r}}},bs=xs(ms),ws=bs.right,Ms=bs.left,Ts=function(t,n){return n<t?-1:n>t?1:n>=t?0:NaN},Ns=function(t){return null===t?NaN:+t},ks=function(t,n){var e,r,i=t.length,o=0,u=0,a=-1,c=0;if(null==n)for(;++a<i;)isNaN(e=Ns(t[a]))||(r=e-o,o+=r/++c,u+=r*(e-o));else for(;++a<i;)isNaN(e=Ns(n(t[a],a,t)))||(r=e-o,o+=r/++c,u+=r*(e-o));if(c>1)return u/(c-1)},Ss=function(t,n){var e=ks(t,n);return e?Math.sqrt(e):e},As=function(t,n){var e,r,i,o=-1,u=t.length;if(null==n){for(;++o<u;)if(null!=(r=t[o])&&r>=r){e=i=r;break}for(;++o<u;)null!=(r=t[o])&&(e>r&&(e=r),i<r&&(i=r))}else{for(;++o<u;)if(null!=(r=n(t[o],o,t))&&r>=r){e=i=r;break}for(;++o<u;)null!=(r=n(t[o],o,t))&&(e>r&&(e=r),i<r&&(i=r))}return[e,i]},Es=Array.prototype,Cs=Es.slice,zs=Es.map,Ps=function(t){return function(){return t}},qs=function(t){return t},Ls=function(t,n,e){t=+t,n=+n,e=(i=arguments.length)<2?(n=t,t=0,1):i<3?1:+e;for(var r=-1,i=0|Math.max(0,Math.ceil((n-t)/e)),o=new Array(i);++r<i;)o[r]=t+r*e;return o},Rs=Math.sqrt(50),Us=Math.sqrt(10),Ds=Math.sqrt(2),Os=function(t,n,r){var i=e(t,n,r);return Ls(Math.ceil(t/i)*i,Math.floor(n/i)*i+i/2,i)},Fs=function(t){return Math.ceil(Math.log(t.length)/Math.LN2)+1},Is=function(){function t(t){var i,o,u=t.length,a=new Array(u);for(i=0;i<u;++i)a[i]=n(t[i],i,t);var c=e(a),s=c[0],f=c[1],l=r(a,s,f);Array.isArray(l)||(l=Os(s,f,l));for(var h=l.length;l[0]<=s;)l.shift(),--h;for(;l[h-1]>=f;)l.pop(),--h;var p,d=new Array(h+1);for(i=0;i<=h;++i)p=d[i]=[],p.x0=i>0?l[i-1]:s,p.x1=i<h?l[i]:f;for(i=0;i<u;++i)o=a[i],s<=o&&o<=f&&d[ws(l,o,0,h)].push(t[i]);return d}var n=qs,e=As,r=Fs;return t.value=function(e){return arguments.length?(n="function"==typeof e?e:Ps(e),t):n},t.domain=function(n){return arguments.length?(e="function"==typeof n?n:Ps([n[0],n[1]]),t):e},t.thresholds=function(n){return arguments.length?(r="function"==typeof n?n:Ps(Array.isArray(n)?Cs.call(n):n),t):r},t},Ys=function(t,n,e){if(null==e&&(e=Ns),r=t.length){if((n=+n)<=0||r<2)return+e(t[0],0,t);if(n>=1)return+e(t[r-1],r-1,t);var r,i=(r-1)*n,o=Math.floor(i),u=+e(t[o],o,t),a=+e(t[o+1],o+1,t);return u+(a-u)*(i-o)}},Bs=function(t,n,e){return t=zs.call(t,Ns).sort(ms),Math.ceil((e-n)/(2*(Ys(t,.75)-Ys(t,.25))*Math.pow(t.length,-1/3)))},js=function(t,n,e){return Math.ceil((e-n)/(3.5*Ss(t)*Math.pow(t.length,-1/3)))},Hs=function(t,n){var e,r,i=-1,o=t.length;if(null==n){for(;++i<o;)if(null!=(r=t[i])&&r>=r){e=r;break}for(;++i<o;)null!=(r=t[i])&&r>e&&(e=r)}else{for(;++i<o;)if(null!=(r=n(t[i],i,t))&&r>=r){e=r;break}for(;++i<o;)null!=(r=n(t[i],i,t))&&r>e&&(e=r)}return e},Xs=function(t,n){var e,r=0,i=t.length,o=-1,u=i;if(null==n)for(;++o<i;)isNaN(e=Ns(t[o]))?--u:r+=e;else for(;++o<i;)isNaN(e=Ns(n(t[o],o,t)))?--u:r+=e;if(u)return r/u},Vs=function(t,n){var e,r=[],i=t.length,o=-1;if(null==n)for(;++o<i;)isNaN(e=Ns(t[o]))||r.push(e);else for(;++o<i;)isNaN(e=Ns(n(t[o],o,t)))||r.push(e);return Ys(r.sort(ms),.5)},Ws=function(t){for(var n,e,r,i=t.length,o=-1,u=0;++o<i;)u+=t[o].length;for(e=new Array(u);--i>=0;)for(r=t[i],n=r.length;--n>=0;)e[--u]=r[n];return e},$s=function(t,n){var e,r,i=-1,o=t.length;if(null==n){for(;++i<o;)if(null!=(r=t[i])&&r>=r){e=r;break}for(;++i<o;)null!=(r=t[i])&&e>r&&(e=r)}else{for(;++i<o;)if(null!=(r=n(t[i],i,t))&&r>=r){e=r;break}for(;++i<o;)null!=(r=n(t[i],i,t))&&e>r&&(e=r)}return e},Zs=function(t){for(var n=0,e=t.length-1,r=t[0],i=new Array(e<0?0:e);n<e;)i[n]=[r,r=t[++n]];return i},Gs=function(t,n){for(var e=n.length,r=new Array(e);e--;)r[e]=t[n[e]];return r},Js=function(t,n){if(e=t.length){var e,r,i=0,o=0,u=t[o];for(n||(n=ms);++i<e;)(n(r=t[i],u)<0||0!==n(u,u))&&(u=r,o=i);return 0===n(u,u)?o:void 0}},Qs=function(t,n,e){for(var r,i,o=(null==e?t.length:e)-(n=null==n?0:+n);o;)i=Math.random()*o--|0,r=t[o+n],t[o+n]=t[i+n],t[i+n]=r;return t},Ks=function(t,n){var e,r=0,i=t.length,o=-1;if(null==n)for(;++o<i;)(e=+t[o])&&(r+=e);else for(;++o<i;)(e=+n(t[o],o,t))&&(r+=e);return r},tf=function(t){if(!(o=t.length))return[];for(var n=-1,e=$s(t,r),i=new Array(e);++n<e;)for(var o,u=-1,a=i[n]=new Array(o);++u<o;)a[u]=t[u][n];return i},nf=function(){return tf(arguments)},ef="$";i.prototype=o.prototype={constructor:i,has:function(t){return ef+t in this},get:function(t){return this[ef+t]},set:function(t,n){return this[ef+t]=n,this},remove:function(t){var n=ef+t;return n in this&&delete this[n]},clear:function(){for(var t in this)t[0]===ef&&delete this[t]},keys:function(){var t=[];for(var n in this)n[0]===ef&&t.push(n.slice(1));return t},values:function(){var t=[];for(var n in this)n[0]===ef&&t.push(this[n]);return t},entries:function(){var t=[];for(var n in this)n[0]===ef&&t.push({key:n.slice(1),value:this[n]});return t},size:function(){var t=0;for(var n in this)n[0]===ef&&++t;return t},empty:function(){for(var t in this)if(t[0]===ef)return!1;return!0},each:function(t){for(var n in this)n[0]===ef&&t(this[n],n.slice(1),this)}};var rf=function(){function t(n,i,u,a){if(i>=f.length)return null!=r?r(n):null!=e?n.sort(e):n;for(var c,s,l,h=-1,p=n.length,d=f[i++],v=o(),_=u();++h<p;)(l=v.get(c=d(s=n[h])+""))?l.push(s):v.set(c,[s]);return v.each(function(n,e){a(_,e,t(n,i,u,a))}),_}function n(t,e){if(++e>f.length)return t;var i,o=l[e-1];return null!=r&&e>=f.length?i=t.entries():(i=[],t.each(function(t,r){i.push({key:r,values:n(t,e)})})),null!=o?i.sort(function(t,n){return o(t.key,n.key)}):i}var e,r,i,f=[],l=[];return i={object:function(n){return t(n,0,u,a)},map:function(n){return t(n,0,c,s)},entries:function(e){return n(t(e,0,c,s),0)},key:function(t){return f.push(t),i},sortKeys:function(t){return l[f.length-1]=t,i},sortValues:function(t){return e=t,i},rollup:function(t){return r=t,i}}},of=o.prototype;f.prototype=l.prototype={constructor:f,has:of.has,add:function(t){return t+="",this[ef+t]=t,this},remove:of.remove,clear:of.clear,values:of.keys,size:of.size,empty:of.empty,each:of.each};var uf=function(t){var n=[];for(var e in t)n.push(e);return n},af=function(t){var n=[];for(var e in t)n.push(t[e]);return n},cf=function(t){var n=[];for(var e in t)n.push({key:e,value:t[e]});return n},sf=function(t,n){return t=null==t?0:+t,n=null==n?1:+n,1===arguments.length?(n=t,t=0):n-=t,function(){return Math.random()*n+t}},ff=function(t,n){var e,r;return t=null==t?0:+t,n=null==n?1:+n,function(){var i;if(null!=e)i=e,e=null;else do e=2*Math.random()-1,i=2*Math.random()-1,r=e*e+i*i;while(!r||r>1);return t+n*i*Math.sqrt(-2*Math.log(r)/r)}},lf=function(){var t=ff.apply(this,arguments);return function(){return Math.exp(t())}},hf=function(t){return function(){for(var n=0,e=0;e<t;++e)n+=Math.random();return n}},pf=function(t){var n=hf(t);return function(){return n()/t}},df=function(t){return function(){return-Math.log(1-Math.random())/t}},vf=3,_f=function t(n){function e(t){return Math.pow(t,n)}return n=+n,e.exponent=t,e}(vf),yf=function t(n){function e(t){return 1-Math.pow(1-t,n)}return n=+n,e.exponent=t,e}(vf),gf=function t(n){function e(t){return((t*=2)<=1?Math.pow(t,n):2-Math.pow(2-t,n))/2}return n=+n,e.exponent=t,e}(vf),mf=Math.PI,xf=mf/2,bf=4/11,wf=6/11,Mf=8/11,Tf=.75,Nf=9/11,kf=10/11,Sf=.9375,Af=21/22,Ef=63/64,Cf=1/bf/bf,zf=1.70158,Pf=function t(n){function e(t){return t*t*((n+1)*t-n)}return n=+n,e.overshoot=t,e}(zf),qf=function t(n){function e(t){return--t*t*((n+1)*t+n)+1}return n=+n,e.overshoot=t,e}(zf),Lf=function t(n){function e(t){return((t*=2)<1?t*t*((n+1)*t-n):(t-=2)*t*((n+1)*t+n)+2)/2}return n=+n,e.overshoot=t,e}(zf),Rf=2*Math.PI,Uf=1,Df=.3,Of=function t(n,e){function r(t){return n*Math.pow(2,10*--t)*Math.sin((i-t)/e)}var i=Math.asin(1/(n=Math.max(1,n)))*(e/=Rf);return r.amplitude=function(n){return t(n,e*Rf)},r.period=function(e){return t(n,e)},r}(Uf,Df),Ff=function t(n,e){function r(t){return 1-n*Math.pow(2,-10*(t=+t))*Math.sin((t+i)/e)}var i=Math.asin(1/(n=Math.max(1,n)))*(e/=Rf);return r.amplitude=function(n){return t(n,e*Rf)},r.period=function(e){return t(n,e)},r}(Uf,Df),If=function t(n,e){function r(t){return((t=2*t-1)<0?n*Math.pow(2,10*t)*Math.sin((i-t)/e):2-n*Math.pow(2,-10*t)*Math.sin((i+t)/e))/2}var i=Math.asin(1/(n=Math.max(1,n)))*(e/=Rf);return r.amplitude=function(n){return t(n,e*Rf)},r.period=function(e){return t(n,e)},r}(Uf,Df),Yf=function(t){for(var n,e=-1,r=t.length,i=t[r-1],o=0;++e<r;)n=i,i=t[e],o+=n[1]*i[0]-n[0]*i[1];return o/2},Bf=function(t){for(var n,e,r=-1,i=t.length,o=0,u=0,a=t[i-1],c=0;++r<i;)n=a,a=t[r],c+=e=n[0]*a[1]-a[0]*n[1],o+=(n[0]+a[0])*e,u+=(n[1]+a[1])*e;return c*=3,[o/c,u/c]},jf=function(t,n,e){return(n[0]-t[0])*(e[1]-t[1])-(n[1]-t[1])*(e[0]-t[0])},Hf=function(t){if((e=t.length)<3)return null;var n,e,r=new Array(e),i=new Array(e);for(n=0;n<e;++n)r[n]=[+t[n][0],+t[n][1],n];for(r.sort(z),n=0;n<e;++n)i[n]=[r[n][0],-r[n][1]];var o=P(r),u=P(i),a=u[0]===o[0],c=u[u.length-1]===o[o.length-1],s=[];for(n=o.length-1;n>=0;--n)s.push(t[r[o[n]][2]]);for(n=+a;n<u.length-c;++n)s.push(t[r[u[n]][2]]);return s},Xf=function(t,n){for(var e,r,i=t.length,o=t[i-1],u=n[0],a=n[1],c=o[0],s=o[1],f=!1,l=0;l<i;++l)o=t[l],e=o[0],r=o[1],r>a!=s>a&&u<(c-e)*(a-r)/(s-r)+e&&(f=!f),c=e,s=r;return f},Vf=function(t){for(var n,e,r=-1,i=t.length,o=t[i-1],u=o[0],a=o[1],c=0;++r<i;)n=u,e=a,o=t[r],u=o[0],a=o[1],n-=u,e-=a,c+=Math.sqrt(n*n+e*e);return c},Wf=Math.PI,$f=2*Wf,Zf=1e-6,Gf=$f-Zf;q.prototype=L.prototype={constructor:q,moveTo:function(t,n){this._.push("M",this._x0=this._x1=+t,",",this._y0=this._y1=+n)},closePath:function(){null!==this._x1&&(this._x1=this._x0,this._y1=this._y0,this._.push("Z"))},lineTo:function(t,n){this._.push("L",this._x1=+t,",",this._y1=+n)},quadraticCurveTo:function(t,n,e,r){this._.push("Q",+t,",",+n,",",this._x1=+e,",",this._y1=+r)},bezierCurveTo:function(t,n,e,r,i,o){this._.push("C",+t,",",+n,",",+e,",",+r,",",this._x1=+i,",",this._y1=+o)},arcTo:function(t,n,e,r,i){t=+t,n=+n,e=+e,r=+r,i=+i;var o=this._x1,u=this._y1,a=e-t,c=r-n,s=o-t,f=u-n,l=s*s+f*f;if(i<0)throw new Error("negative radius: "+i);if(null===this._x1)this._.push("M",this._x1=t,",",this._y1=n);else if(l>Zf)if(Math.abs(f*a-c*s)>Zf&&i){var h=e-o,p=r-u,d=a*a+c*c,v=h*h+p*p,_=Math.sqrt(d),y=Math.sqrt(l),g=i*Math.tan((Wf-Math.acos((d+l-v)/(2*_*y)))/2),m=g/y,x=g/_;Math.abs(m-1)>Zf&&this._.push("L",t+m*s,",",n+m*f),this._.push("A",i,",",i,",0,0,",+(f*h>s*p),",",this._x1=t+x*a,",",this._y1=n+x*c)}else this._.push("L",this._x1=t,",",this._y1=n);else;},arc:function(t,n,e,r,i,o){t=+t,n=+n,e=+e;var u=e*Math.cos(r),a=e*Math.sin(r),c=t+u,s=n+a,f=1^o,l=o?r-i:i-r;if(e<0)throw new Error("negative radius: "+e);null===this._x1?this._.push("M",c,",",s):(Math.abs(this._x1-c)>Zf||Math.abs(this._y1-s)>Zf)&&this._.push("L",c,",",s),e&&(l>Gf?this._.push("A",e,",",e,",0,1,",f,",",t-u,",",n-a,"A",e,",",e,",0,1,",f,",",this._x1=c,",",this._y1=s):(l<0&&(l=l%$f+$f),this._.push("A",e,",",e,",0,",+(l>=Wf),",",f,",",this._x1=t+e*Math.cos(i),",",this._y1=n+e*Math.sin(i))))},rect:function(t,n,e,r){this._.push("M",this._x0=this._x1=+t,",",this._y0=this._y1=+n,"h",+e,"v",+r,"h",-e,"Z")},toString:function(){return this._.join("")}};var Jf=function(t){var n=+this._x.call(null,t),e=+this._y.call(null,t);return R(this.cover(n,e),n,e,t)},Qf=function(t,n){if(isNaN(t=+t)||isNaN(n=+n))return this;var e=this._x0,r=this._y0,i=this._x1,o=this._y1;if(isNaN(e))i=(e=Math.floor(t))+1,o=(r=Math.floor(n))+1;else{if(!(e>t||t>i||r>n||n>o))return this;var u,a,c=i-e,s=this._root;switch(a=(n<(r+o)/2)<<1|t<(e+i)/2){case 0:do u=new Array(4),u[a]=s,s=u;while(c*=2,i=e+c,o=r+c,t>i||n>o);break;case 1:do u=new Array(4),u[a]=s,s=u;while(c*=2,e=i-c,o=r+c,e>t||n>o);break;case 2:do u=new Array(4),u[a]=s,s=u;while(c*=2,i=e+c,r=o-c,t>i||r>n);break;case 3:do u=new Array(4),u[a]=s,s=u;while(c*=2,e=i-c,r=o-c,e>t||r>n)}this._root&&this._root.length&&(this._root=s)}return this._x0=e,this._y0=r,this._x1=i,this._y1=o,this},Kf=function(){var t=[];return this.visit(function(n){if(!n.length)do t.push(n.data);while(n=n.next)}),t},tl=function(t){return arguments.length?this.cover(+t[0][0],+t[0][1]).cover(+t[1][0],+t[1][1]):isNaN(this._x0)?void 0:[[this._x0,this._y0],[this._x1,this._y1]]},nl=function(t,n,e,r,i){this.node=t,this.x0=n,this.y0=e,this.x1=r,this.y1=i},el=function(t,n,e){var r,i,o,u,a,c,s,f=this._x0,l=this._y0,h=this._x1,p=this._y1,d=[],v=this._root;for(v&&d.push(new nl(v,f,l,h,p)),null==e?e=1/0:(f=t-e,l=n-e,h=t+e,p=n+e,e*=e);c=d.pop();)if(!(!(v=c.node)||(i=c.x0)>h||(o=c.y0)>p||(u=c.x1)<f||(a=c.y1)<l))if(v.length){var _=(i+u)/2,y=(o+a)/2;d.push(new nl(v[3],_,y,u,a),new nl(v[2],i,y,_,a),new nl(v[1],_,o,u,y),new nl(v[0],i,o,_,y)),(s=(n>=y)<<1|t>=_)&&(c=d[d.length-1],d[d.length-1]=d[d.length-1-s],d[d.length-1-s]=c)}else{var g=t-+this._x.call(null,v.data),m=n-+this._y.call(null,v.data),x=g*g+m*m;if(x<e){var b=Math.sqrt(e=x);f=t-b,l=n-b,h=t+b,p=n+b,r=v.data}}return r},rl=function(t){if(isNaN(o=+this._x.call(null,t))||isNaN(u=+this._y.call(null,t)))return this;var n,e,r,i,o,u,a,c,s,f,l,h,p=this._root,d=this._x0,v=this._y0,_=this._x1,y=this._y1;if(!p)return this;if(p.length)for(;;){if((s=o>=(a=(d+_)/2))?d=a:_=a,(f=u>=(c=(v+y)/2))?v=c:y=c,n=p,!(p=p[l=f<<1|s]))return this;if(!p.length)break;(n[l+1&3]||n[l+2&3]||n[l+3&3])&&(e=n,h=l)}for(;p.data!==t;)if(r=p,!(p=p.next))return this;return(i=p.next)&&delete p.next,r?(i?r.next=i:delete r.next,this):n?(i?n[l]=i:delete n[l],(p=n[0]||n[1]||n[2]||n[3])&&p===(n[3]||n[2]||n[1]||n[0])&&!p.length&&(e?e[h]=p:this._root=p),this):(this._root=i,this)},il=function(){return this._root},ol=function(){var t=0;return this.visit(function(n){if(!n.length)do++t;while(n=n.next)}),t},ul=function(t){var n,e,r,i,o,u,a=[],c=this._root;for(c&&a.push(new nl(c,this._x0,this._y0,this._x1,this._y1));n=a.pop();)if(!t(c=n.node,r=n.x0,i=n.y0,o=n.x1,u=n.y1)&&c.length){var s=(r+o)/2,f=(i+u)/2;(e=c[3])&&a.push(new nl(e,s,f,o,u)),(e=c[2])&&a.push(new nl(e,r,f,s,u)),(e=c[1])&&a.push(new nl(e,s,i,o,f)),(e=c[0])&&a.push(new nl(e,r,i,s,f))}return this},al=function(t){var n,e=[],r=[];for(this._root&&e.push(new nl(this._root,this._x0,this._y0,this._x1,this._y1));n=e.pop();){var i=n.node;if(i.length){var o,u=n.x0,a=n.y0,c=n.x1,s=n.y1,f=(u+c)/2,l=(a+s)/2;(o=i[0])&&e.push(new nl(o,u,a,f,l)),(o=i[1])&&e.push(new nl(o,f,a,c,l)),(o=i[2])&&e.push(new nl(o,u,l,f,s)),(o=i[3])&&e.push(new nl(o,f,l,c,s))}r.push(n)}for(;n=r.pop();)t(n.node,n.x0,n.y0,n.x1,n.y1);return this},cl=function(t){return arguments.length?(this._x=t,this):this._x},sl=function(t){return arguments.length?(this._y=t,this):this._y},fl=I.prototype=Y.prototype;fl.copy=function(){var t,n,e=new Y(this._x,this._y,this._x0,this._y0,this._x1,this._y1),r=this._root;if(!r)return e;if(!r.length)return e._root=B(r),e;for(t=[{source:r,target:e._root=new Array(4)}];r=t.pop();)for(var i=0;i<4;++i)(n=r.source[i])&&(n.length?t.push({source:n,target:r.target[i]=new Array(4)}):r.target[i]=B(n));return e},fl.add=Jf,fl.addAll=U,fl.cover=Qf,fl.data=Kf,fl.extent=tl,fl.find=el,fl.remove=rl,fl.removeAll=D,fl.root=il,fl.size=ol,fl.visit=ul,fl.visitAfter=al,fl.x=cl,fl.y=sl;var ll=[].slice,hl={};j.prototype=Z.prototype={constructor:j,defer:function(t){if("function"!=typeof t||this._call)throw new Error;if(null!=this._error)return this;var n=ll.call(arguments,1);return n.push(t),++this._waiting,this._tasks.push(n),H(this),this},abort:function(){return null==this._error&&W(this,new Error("abort")),this},await:function(t){if("function"!=typeof t||this._call)throw new Error;return this._call=function(n,e){t.apply(null,[n].concat(e))},$(this),this},awaitAll:function(t){if("function"!=typeof t||this._call)throw new Error;return this._call=t,$(this),this}};var pl=function(t){return function(){return t}},dl=1e-12,vl=Math.PI,_l=vl/2,yl=2*vl,gl=function(){function t(){var t,s,f=+n.apply(this,arguments),l=+e.apply(this,arguments),h=o.apply(this,arguments)-_l,p=u.apply(this,arguments)-_l,d=Math.abs(p-h),v=p>h;if(c||(c=t=L()),l<f&&(s=l,l=f,f=s),l>dl)if(d>yl-dl)c.moveTo(l*Math.cos(h),l*Math.sin(h)),c.arc(0,0,l,h,p,!v),f>dl&&(c.moveTo(f*Math.cos(p),f*Math.sin(p)),c.arc(0,0,f,p,h,v));else{var _,y,g=h,m=p,x=h,b=p,w=d,M=d,T=a.apply(this,arguments)/2,N=T>dl&&(i?+i.apply(this,arguments):Math.sqrt(f*f+l*l)),k=Math.min(Math.abs(l-f)/2,+r.apply(this,arguments)),S=k,A=k;if(N>dl){var E=nt(N/f*Math.sin(T)),C=nt(N/l*Math.sin(T));(w-=2*E)>dl?(E*=v?1:-1,x+=E,b-=E):(w=0,x=b=(h+p)/2),(M-=2*C)>dl?(C*=v?1:-1,g+=C,m-=C):(M=0,g=m=(h+p)/2)}var z=l*Math.cos(g),P=l*Math.sin(g),q=f*Math.cos(b),R=f*Math.sin(b);if(k>dl){var U=l*Math.cos(m),D=l*Math.sin(m),O=f*Math.cos(x),F=f*Math.sin(x);if(d<vl){var I=w>dl?et(z,P,O,F,U,D,q,R):[q,R],Y=z-I[0],B=P-I[1],j=U-I[0],H=D-I[1],X=1/Math.sin(Math.acos((Y*j+B*H)/(Math.sqrt(Y*Y+B*B)*Math.sqrt(j*j+H*H)))/2),V=Math.sqrt(I[0]*I[0]+I[1]*I[1]);S=Math.min(k,(f-V)/(X-1)),A=Math.min(k,(l-V)/(X+1))}}M>dl?A>dl?(_=rt(O,F,z,P,l,A,v),y=rt(U,D,q,R,l,A,v),c.moveTo(_.cx+_.x01,_.cy+_.y01),A<k?c.arc(_.cx,_.cy,A,Math.atan2(_.y01,_.x01),Math.atan2(y.y01,y.x01),!v):(c.arc(_.cx,_.cy,A,Math.atan2(_.y01,_.x01),Math.atan2(_.y11,_.x11),!v),c.arc(0,0,l,Math.atan2(_.cy+_.y11,_.cx+_.x11),Math.atan2(y.cy+y.y11,y.cx+y.x11),!v),c.arc(y.cx,y.cy,A,Math.atan2(y.y11,y.x11),Math.atan2(y.y01,y.x01),!v))):(c.moveTo(z,P),c.arc(0,0,l,g,m,!v)):c.moveTo(z,P),f>dl&&w>dl?S>dl?(_=rt(q,R,U,D,f,-S,v),y=rt(z,P,O,F,f,-S,v),c.lineTo(_.cx+_.x01,_.cy+_.y01),S<k?c.arc(_.cx,_.cy,S,Math.atan2(_.y01,_.x01),Math.atan2(y.y01,y.x01),!v):(c.arc(_.cx,_.cy,S,Math.atan2(_.y01,_.x01),Math.atan2(_.y11,_.x11),!v),
c.arc(0,0,f,Math.atan2(_.cy+_.y11,_.cx+_.x11),Math.atan2(y.cy+y.y11,y.cx+y.x11),v),c.arc(y.cx,y.cy,S,Math.atan2(y.y11,y.x11),Math.atan2(y.y01,y.x01),!v))):c.arc(0,0,f,b,x,v):c.lineTo(q,R)}else c.moveTo(0,0);if(c.closePath(),t)return c=null,t+""||null}var n=G,e=J,r=pl(0),i=null,o=Q,u=K,a=tt,c=null;return t.centroid=function(){var t=(+n.apply(this,arguments)+ +e.apply(this,arguments))/2,r=(+o.apply(this,arguments)+ +u.apply(this,arguments))/2-vl/2;return[Math.cos(r)*t,Math.sin(r)*t]},t.innerRadius=function(e){return arguments.length?(n="function"==typeof e?e:pl(+e),t):n},t.outerRadius=function(n){return arguments.length?(e="function"==typeof n?n:pl(+n),t):e},t.cornerRadius=function(n){return arguments.length?(r="function"==typeof n?n:pl(+n),t):r},t.padRadius=function(n){return arguments.length?(i=null==n?null:"function"==typeof n?n:pl(+n),t):i},t.startAngle=function(n){return arguments.length?(o="function"==typeof n?n:pl(+n),t):o},t.endAngle=function(n){return arguments.length?(u="function"==typeof n?n:pl(+n),t):u},t.padAngle=function(n){return arguments.length?(a="function"==typeof n?n:pl(+n),t):a},t.context=function(n){return arguments.length?(c=null==n?null:n,t):c},t};it.prototype={areaStart:function(){this._line=0},areaEnd:function(){this._line=NaN},lineStart:function(){this._point=0},lineEnd:function(){(this._line||0!==this._line&&1===this._point)&&this._context.closePath(),this._line=1-this._line},point:function(t,n){switch(t=+t,n=+n,this._point){case 0:this._point=1,this._line?this._context.lineTo(t,n):this._context.moveTo(t,n);break;case 1:this._point=2;default:this._context.lineTo(t,n)}}};var ml=function(t){return new it(t)},xl=function(){function t(t){var a,c,s,f=t.length,l=!1;for(null==i&&(u=o(s=L())),a=0;a<=f;++a)!(a<f&&r(c=t[a],a,t))===l&&((l=!l)?u.lineStart():u.lineEnd()),l&&u.point(+n(c,a,t),+e(c,a,t));if(s)return u=null,s+""||null}var n=ot,e=ut,r=pl(!0),i=null,o=ml,u=null;return t.x=function(e){return arguments.length?(n="function"==typeof e?e:pl(+e),t):n},t.y=function(n){return arguments.length?(e="function"==typeof n?n:pl(+n),t):e},t.defined=function(n){return arguments.length?(r="function"==typeof n?n:pl(!!n),t):r},t.curve=function(n){return arguments.length?(o=n,null!=i&&(u=o(i)),t):o},t.context=function(n){return arguments.length?(null==n?i=u=null:u=o(i=n),t):i},t},bl=function(){function t(t){var n,f,l,h,p,d=t.length,v=!1,_=new Array(d),y=new Array(d);for(null==a&&(s=c(p=L())),n=0;n<=d;++n){if(!(n<d&&u(h=t[n],n,t))===v)if(v=!v)f=n,s.areaStart(),s.lineStart();else{for(s.lineEnd(),s.lineStart(),l=n-1;l>=f;--l)s.point(_[l],y[l]);s.lineEnd(),s.areaEnd()}v&&(_[n]=+e(h,n,t),y[n]=+i(h,n,t),s.point(r?+r(h,n,t):_[n],o?+o(h,n,t):y[n]))}if(p)return s=null,p+""||null}function n(){return xl().defined(u).curve(c).context(a)}var e=ot,r=null,i=pl(0),o=ut,u=pl(!0),a=null,c=ml,s=null;return t.x=function(n){return arguments.length?(e="function"==typeof n?n:pl(+n),r=null,t):e},t.x0=function(n){return arguments.length?(e="function"==typeof n?n:pl(+n),t):e},t.x1=function(n){return arguments.length?(r=null==n?null:"function"==typeof n?n:pl(+n),t):r},t.y=function(n){return arguments.length?(i="function"==typeof n?n:pl(+n),o=null,t):i},t.y0=function(n){return arguments.length?(i="function"==typeof n?n:pl(+n),t):i},t.y1=function(n){return arguments.length?(o=null==n?null:"function"==typeof n?n:pl(+n),t):o},t.lineX0=t.lineY0=function(){return n().x(e).y(i)},t.lineY1=function(){return n().x(e).y(o)},t.lineX1=function(){return n().x(r).y(i)},t.defined=function(n){return arguments.length?(u="function"==typeof n?n:pl(!!n),t):u},t.curve=function(n){return arguments.length?(c=n,null!=a&&(s=c(a)),t):c},t.context=function(n){return arguments.length?(null==n?a=s=null:s=c(a=n),t):a},t},wl=function(t,n){return n<t?-1:n>t?1:n>=t?0:NaN},Ml=function(t){return t},Tl=function(){function t(t){var a,c,s,f,l,h=t.length,p=0,d=new Array(h),v=new Array(h),_=+i.apply(this,arguments),y=Math.min(yl,Math.max(-yl,o.apply(this,arguments)-_)),g=Math.min(Math.abs(y)/h,u.apply(this,arguments)),m=g*(y<0?-1:1);for(a=0;a<h;++a)(l=v[d[a]=a]=+n(t[a],a,t))>0&&(p+=l);for(null!=e?d.sort(function(t,n){return e(v[t],v[n])}):null!=r&&d.sort(function(n,e){return r(t[n],t[e])}),a=0,s=p?(y-h*m)/p:0;a<h;++a,_=f)c=d[a],l=v[c],f=_+(l>0?l*s:0)+m,v[c]={data:t[c],index:a,value:l,startAngle:_,endAngle:f,padAngle:g};return v}var n=Ml,e=wl,r=null,i=pl(0),o=pl(yl),u=pl(0);return t.value=function(e){return arguments.length?(n="function"==typeof e?e:pl(+e),t):n},t.sortValues=function(n){return arguments.length?(e=n,r=null,t):e},t.sort=function(n){return arguments.length?(r=n,e=null,t):r},t.startAngle=function(n){return arguments.length?(i="function"==typeof n?n:pl(+n),t):i},t.endAngle=function(n){return arguments.length?(o="function"==typeof n?n:pl(+n),t):o},t.padAngle=function(n){return arguments.length?(u="function"==typeof n?n:pl(+n),t):u},t},Nl=ct(ml);at.prototype={areaStart:function(){this._curve.areaStart()},areaEnd:function(){this._curve.areaEnd()},lineStart:function(){this._curve.lineStart()},lineEnd:function(){this._curve.lineEnd()},point:function(t,n){this._curve.point(n*Math.sin(t),n*-Math.cos(t))}};var kl=function(){return st(xl().curve(Nl))},Sl=function(){var t=bl().curve(Nl),n=t.curve,e=t.lineX0,r=t.lineX1,i=t.lineY0,o=t.lineY1;return t.angle=t.x,delete t.x,t.startAngle=t.x0,delete t.x0,t.endAngle=t.x1,delete t.x1,t.radius=t.y,delete t.y,t.innerRadius=t.y0,delete t.y0,t.outerRadius=t.y1,delete t.y1,t.lineStartAngle=function(){return st(e())},delete t.lineX0,t.lineEndAngle=function(){return st(r())},delete t.lineX1,t.lineInnerRadius=function(){return st(i())},delete t.lineY0,t.lineOuterRadius=function(){return st(o())},delete t.lineY1,t.curve=function(t){return arguments.length?n(ct(t)):n()._curve},t},Al={draw:function(t,n){var e=Math.sqrt(n/vl);t.moveTo(e,0),t.arc(0,0,e,0,yl)}},El={draw:function(t,n){var e=Math.sqrt(n/5)/2;t.moveTo(-3*e,-e),t.lineTo(-e,-e),t.lineTo(-e,-3*e),t.lineTo(e,-3*e),t.lineTo(e,-e),t.lineTo(3*e,-e),t.lineTo(3*e,e),t.lineTo(e,e),t.lineTo(e,3*e),t.lineTo(-e,3*e),t.lineTo(-e,e),t.lineTo(-3*e,e),t.closePath()}},Cl=Math.sqrt(1/3),zl=2*Cl,Pl={draw:function(t,n){var e=Math.sqrt(n/zl),r=e*Cl;t.moveTo(0,-e),t.lineTo(r,0),t.lineTo(0,e),t.lineTo(-r,0),t.closePath()}},ql=.8908130915292852,Ll=Math.sin(vl/10)/Math.sin(7*vl/10),Rl=Math.sin(yl/10)*Ll,Ul=-Math.cos(yl/10)*Ll,Dl={draw:function(t,n){var e=Math.sqrt(n*ql),r=Rl*e,i=Ul*e;t.moveTo(0,-e),t.lineTo(r,i);for(var o=1;o<5;++o){var u=yl*o/5,a=Math.cos(u),c=Math.sin(u);t.lineTo(c*e,-a*e),t.lineTo(a*r-c*i,c*r+a*i)}t.closePath()}},Ol={draw:function(t,n){var e=Math.sqrt(n),r=-e/2;t.rect(r,r,e,e)}},Fl=Math.sqrt(3),Il={draw:function(t,n){var e=-Math.sqrt(n/(3*Fl));t.moveTo(0,2*e),t.lineTo(-Fl*e,-e),t.lineTo(Fl*e,-e),t.closePath()}},Yl=-.5,Bl=Math.sqrt(3)/2,jl=1/Math.sqrt(12),Hl=3*(jl/2+1),Xl={draw:function(t,n){var e=Math.sqrt(n/Hl),r=e/2,i=e*jl,o=r,u=e*jl+e,a=-o,c=u;t.moveTo(r,i),t.lineTo(o,u),t.lineTo(a,c),t.lineTo(Yl*r-Bl*i,Bl*r+Yl*i),t.lineTo(Yl*o-Bl*u,Bl*o+Yl*u),t.lineTo(Yl*a-Bl*c,Bl*a+Yl*c),t.lineTo(Yl*r+Bl*i,Yl*i-Bl*r),t.lineTo(Yl*o+Bl*u,Yl*u-Bl*o),t.lineTo(Yl*a+Bl*c,Yl*c-Bl*a),t.closePath()}},Vl=[Al,El,Pl,Ol,Dl,Il,Xl],Wl=function(){function t(){var t;if(r||(r=t=L()),n.apply(this,arguments).draw(r,+e.apply(this,arguments)),t)return r=null,t+""||null}var n=pl(Al),e=pl(64),r=null;return t.type=function(e){return arguments.length?(n="function"==typeof e?e:pl(e),t):n},t.size=function(n){return arguments.length?(e="function"==typeof n?n:pl(+n),t):e},t.context=function(n){return arguments.length?(r=null==n?null:n,t):r},t},$l=function(){};lt.prototype={areaStart:function(){this._line=0},areaEnd:function(){this._line=NaN},lineStart:function(){this._x0=this._x1=this._y0=this._y1=NaN,this._point=0},lineEnd:function(){switch(this._point){case 3:ft(this,this._x1,this._y1);case 2:this._context.lineTo(this._x1,this._y1)}(this._line||0!==this._line&&1===this._point)&&this._context.closePath(),this._line=1-this._line},point:function(t,n){switch(t=+t,n=+n,this._point){case 0:this._point=1,this._line?this._context.lineTo(t,n):this._context.moveTo(t,n);break;case 1:this._point=2;break;case 2:this._point=3,this._context.lineTo((5*this._x0+this._x1)/6,(5*this._y0+this._y1)/6);default:ft(this,t,n)}this._x0=this._x1,this._x1=t,this._y0=this._y1,this._y1=n}};var Zl=function(t){return new lt(t)};ht.prototype={areaStart:$l,areaEnd:$l,lineStart:function(){this._x0=this._x1=this._x2=this._x3=this._x4=this._y0=this._y1=this._y2=this._y3=this._y4=NaN,this._point=0},lineEnd:function(){switch(this._point){case 1:this._context.moveTo(this._x2,this._y2),this._context.closePath();break;case 2:this._context.moveTo((this._x2+2*this._x3)/3,(this._y2+2*this._y3)/3),this._context.lineTo((this._x3+2*this._x2)/3,(this._y3+2*this._y2)/3),this._context.closePath();break;case 3:this.point(this._x2,this._y2),this.point(this._x3,this._y3),this.point(this._x4,this._y4)}},point:function(t,n){switch(t=+t,n=+n,this._point){case 0:this._point=1,this._x2=t,this._y2=n;break;case 1:this._point=2,this._x3=t,this._y3=n;break;case 2:this._point=3,this._x4=t,this._y4=n,this._context.moveTo((this._x0+4*this._x1+t)/6,(this._y0+4*this._y1+n)/6);break;default:ft(this,t,n)}this._x0=this._x1,this._x1=t,this._y0=this._y1,this._y1=n}};var Gl=function(t){return new ht(t)};pt.prototype={areaStart:function(){this._line=0},areaEnd:function(){this._line=NaN},lineStart:function(){this._x0=this._x1=this._y0=this._y1=NaN,this._point=0},lineEnd:function(){(this._line||0!==this._line&&3===this._point)&&this._context.closePath(),this._line=1-this._line},point:function(t,n){switch(t=+t,n=+n,this._point){case 0:this._point=1;break;case 1:this._point=2;break;case 2:this._point=3;var e=(this._x0+4*this._x1+t)/6,r=(this._y0+4*this._y1+n)/6;this._line?this._context.lineTo(e,r):this._context.moveTo(e,r);break;case 3:this._point=4;default:ft(this,t,n)}this._x0=this._x1,this._x1=t,this._y0=this._y1,this._y1=n}};var Jl=function(t){return new pt(t)};dt.prototype={lineStart:function(){this._x=[],this._y=[],this._basis.lineStart()},lineEnd:function(){var t=this._x,n=this._y,e=t.length-1;if(e>0)for(var r,i=t[0],o=n[0],u=t[e]-i,a=n[e]-o,c=-1;++c<=e;)r=c/e,this._basis.point(this._beta*t[c]+(1-this._beta)*(i+r*u),this._beta*n[c]+(1-this._beta)*(o+r*a));this._x=this._y=null,this._basis.lineEnd()},point:function(t,n){this._x.push(+t),this._y.push(+n)}};var Ql=function t(n){function e(t){return 1===n?new lt(t):new dt(t,n)}return e.beta=function(n){return t(+n)},e}(.85);_t.prototype={areaStart:function(){this._line=0},areaEnd:function(){this._line=NaN},lineStart:function(){this._x0=this._x1=this._x2=this._y0=this._y1=this._y2=NaN,this._point=0},lineEnd:function(){switch(this._point){case 2:this._context.lineTo(this._x2,this._y2);break;case 3:vt(this,this._x1,this._y1)}(this._line||0!==this._line&&1===this._point)&&this._context.closePath(),this._line=1-this._line},point:function(t,n){switch(t=+t,n=+n,this._point){case 0:this._point=1,this._line?this._context.lineTo(t,n):this._context.moveTo(t,n);break;case 1:this._point=2,this._x1=t,this._y1=n;break;case 2:this._point=3;default:vt(this,t,n)}this._x0=this._x1,this._x1=this._x2,this._x2=t,this._y0=this._y1,this._y1=this._y2,this._y2=n}};var Kl=function t(n){function e(t){return new _t(t,n)}return e.tension=function(n){return t(+n)},e}(0);yt.prototype={areaStart:$l,areaEnd:$l,lineStart:function(){this._x0=this._x1=this._x2=this._x3=this._x4=this._x5=this._y0=this._y1=this._y2=this._y3=this._y4=this._y5=NaN,this._point=0},lineEnd:function(){switch(this._point){case 1:this._context.moveTo(this._x3,this._y3),this._context.closePath();break;case 2:this._context.lineTo(this._x3,this._y3),this._context.closePath();break;case 3:this.point(this._x3,this._y3),this.point(this._x4,this._y4),this.point(this._x5,this._y5)}},point:function(t,n){switch(t=+t,n=+n,this._point){case 0:this._point=1,this._x3=t,this._y3=n;break;case 1:this._point=2,this._context.moveTo(this._x4=t,this._y4=n);break;case 2:this._point=3,this._x5=t,this._y5=n;break;default:vt(this,t,n)}this._x0=this._x1,this._x1=this._x2,this._x2=t,this._y0=this._y1,this._y1=this._y2,this._y2=n}};var th=function t(n){function e(t){return new yt(t,n)}return e.tension=function(n){return t(+n)},e}(0);gt.prototype={areaStart:function(){this._line=0},areaEnd:function(){this._line=NaN},lineStart:function(){this._x0=this._x1=this._x2=this._y0=this._y1=this._y2=NaN,this._point=0},lineEnd:function(){(this._line||0!==this._line&&3===this._point)&&this._context.closePath(),this._line=1-this._line},point:function(t,n){switch(t=+t,n=+n,this._point){case 0:this._point=1;break;case 1:this._point=2;break;case 2:this._point=3,this._line?this._context.lineTo(this._x2,this._y2):this._context.moveTo(this._x2,this._y2);break;case 3:this._point=4;default:vt(this,t,n)}this._x0=this._x1,this._x1=this._x2,this._x2=t,this._y0=this._y1,this._y1=this._y2,this._y2=n}};var nh=function t(n){function e(t){return new gt(t,n)}return e.tension=function(n){return t(+n)},e}(0);xt.prototype={areaStart:function(){this._line=0},areaEnd:function(){this._line=NaN},lineStart:function(){this._x0=this._x1=this._x2=this._y0=this._y1=this._y2=NaN,this._l01_a=this._l12_a=this._l23_a=this._l01_2a=this._l12_2a=this._l23_2a=this._point=0},lineEnd:function(){switch(this._point){case 2:this._context.lineTo(this._x2,this._y2);break;case 3:this.point(this._x2,this._y2)}(this._line||0!==this._line&&1===this._point)&&this._context.closePath(),this._line=1-this._line},point:function(t,n){if(t=+t,n=+n,this._point){var e=this._x2-t,r=this._y2-n;this._l23_a=Math.sqrt(this._l23_2a=Math.pow(e*e+r*r,this._alpha))}switch(this._point){case 0:this._point=1,this._line?this._context.lineTo(t,n):this._context.moveTo(t,n);break;case 1:this._point=2;break;case 2:this._point=3;default:mt(this,t,n)}this._l01_a=this._l12_a,this._l12_a=this._l23_a,this._l01_2a=this._l12_2a,this._l12_2a=this._l23_2a,this._x0=this._x1,this._x1=this._x2,this._x2=t,this._y0=this._y1,this._y1=this._y2,this._y2=n}};var eh=function t(n){function e(t){return n?new xt(t,n):new _t(t,0)}return e.alpha=function(n){return t(+n)},e}(.5);bt.prototype={areaStart:$l,areaEnd:$l,lineStart:function(){this._x0=this._x1=this._x2=this._x3=this._x4=this._x5=this._y0=this._y1=this._y2=this._y3=this._y4=this._y5=NaN,this._l01_a=this._l12_a=this._l23_a=this._l01_2a=this._l12_2a=this._l23_2a=this._point=0},lineEnd:function(){switch(this._point){case 1:this._context.moveTo(this._x3,this._y3),this._context.closePath();break;case 2:this._context.lineTo(this._x3,this._y3),this._context.closePath();break;case 3:this.point(this._x3,this._y3),this.point(this._x4,this._y4),this.point(this._x5,this._y5)}},point:function(t,n){if(t=+t,n=+n,this._point){var e=this._x2-t,r=this._y2-n;this._l23_a=Math.sqrt(this._l23_2a=Math.pow(e*e+r*r,this._alpha))}switch(this._point){case 0:this._point=1,this._x3=t,this._y3=n;break;case 1:this._point=2,this._context.moveTo(this._x4=t,this._y4=n);break;case 2:this._point=3,this._x5=t,this._y5=n;break;default:mt(this,t,n)}this._l01_a=this._l12_a,this._l12_a=this._l23_a,this._l01_2a=this._l12_2a,this._l12_2a=this._l23_2a,this._x0=this._x1,this._x1=this._x2,this._x2=t,this._y0=this._y1,this._y1=this._y2,this._y2=n}};var rh=function t(n){function e(t){return n?new bt(t,n):new yt(t,0)}return e.alpha=function(n){return t(+n)},e}(.5);wt.prototype={areaStart:function(){this._line=0},areaEnd:function(){this._line=NaN},lineStart:function(){this._x0=this._x1=this._x2=this._y0=this._y1=this._y2=NaN,this._l01_a=this._l12_a=this._l23_a=this._l01_2a=this._l12_2a=this._l23_2a=this._point=0},lineEnd:function(){(this._line||0!==this._line&&3===this._point)&&this._context.closePath(),this._line=1-this._line},point:function(t,n){if(t=+t,n=+n,this._point){var e=this._x2-t,r=this._y2-n;this._l23_a=Math.sqrt(this._l23_2a=Math.pow(e*e+r*r,this._alpha))}switch(this._point){case 0:this._point=1;break;case 1:this._point=2;break;case 2:this._point=3,this._line?this._context.lineTo(this._x2,this._y2):this._context.moveTo(this._x2,this._y2);break;case 3:this._point=4;default:mt(this,t,n)}this._l01_a=this._l12_a,this._l12_a=this._l23_a,this._l01_2a=this._l12_2a,this._l12_2a=this._l23_2a,this._x0=this._x1,this._x1=this._x2,this._x2=t,this._y0=this._y1,this._y1=this._y2,this._y2=n}};var ih=function t(n){function e(t){return n?new wt(t,n):new gt(t,0)}return e.alpha=function(n){return t(+n)},e}(.5);Mt.prototype={areaStart:$l,areaEnd:$l,lineStart:function(){this._point=0},lineEnd:function(){this._point&&this._context.closePath()},point:function(t,n){t=+t,n=+n,this._point?this._context.lineTo(t,n):(this._point=1,this._context.moveTo(t,n))}};var oh=function(t){return new Mt(t)};At.prototype={areaStart:function(){this._line=0},areaEnd:function(){this._line=NaN},lineStart:function(){this._x0=this._x1=this._y0=this._y1=this._t0=NaN,this._point=0},lineEnd:function(){switch(this._point){case 2:this._context.lineTo(this._x1,this._y1);break;case 3:St(this,this._t0,kt(this,this._t0))}(this._line||0!==this._line&&1===this._point)&&this._context.closePath(),this._line=1-this._line},point:function(t,n){var e=NaN;if(t=+t,n=+n,t!==this._x1||n!==this._y1){switch(this._point){case 0:this._point=1,this._line?this._context.lineTo(t,n):this._context.moveTo(t,n);break;case 1:this._point=2;break;case 2:this._point=3,St(this,kt(this,e=Nt(this,t,n)),e);break;default:St(this,this._t0,e=Nt(this,t,n))}this._x0=this._x1,this._x1=t,this._y0=this._y1,this._y1=n,this._t0=e}}},(Et.prototype=Object.create(At.prototype)).point=function(t,n){At.prototype.point.call(this,n,t)},Ct.prototype={moveTo:function(t,n){this._context.moveTo(n,t)},closePath:function(){this._context.closePath()},lineTo:function(t,n){this._context.lineTo(n,t)},bezierCurveTo:function(t,n,e,r,i,o){this._context.bezierCurveTo(n,t,r,e,o,i)}},qt.prototype={areaStart:function(){this._line=0},areaEnd:function(){this._line=NaN},lineStart:function(){this._x=[],this._y=[]},lineEnd:function(){var t=this._x,n=this._y,e=t.length;if(e)if(this._line?this._context.lineTo(t[0],n[0]):this._context.moveTo(t[0],n[0]),2===e)this._context.lineTo(t[1],n[1]);else for(var r=Lt(t),i=Lt(n),o=0,u=1;u<e;++o,++u)this._context.bezierCurveTo(r[0][o],i[0][o],r[1][o],i[1][o],t[u],n[u]);(this._line||0!==this._line&&1===e)&&this._context.closePath(),this._line=1-this._line,this._x=this._y=null},point:function(t,n){this._x.push(+t),this._y.push(+n)}};var uh=function(t){return new qt(t)};Rt.prototype={areaStart:function(){this._line=0},areaEnd:function(){this._line=NaN},lineStart:function(){this._x=this._y=NaN,this._point=0},lineEnd:function(){0<this._t&&this._t<1&&2===this._point&&this._context.lineTo(this._x,this._y),(this._line||0!==this._line&&1===this._point)&&this._context.closePath(),this._line>=0&&(this._t=1-this._t,this._line=1-this._line)},point:function(t,n){switch(t=+t,n=+n,this._point){case 0:this._point=1,this._line?this._context.lineTo(t,n):this._context.moveTo(t,n);break;case 1:this._point=2;default:if(this._t<=0)this._context.lineTo(this._x,n),this._context.lineTo(t,n);else{var e=this._x*(1-this._t)+t*this._t;this._context.lineTo(e,this._y),this._context.lineTo(e,n)}}this._x=t,this._y=n}};var ah=function(t){return new Rt(t,.5)},ch=Array.prototype.slice,sh=function(t,n){if((r=t.length)>1)for(var e,r,i=1,o=t[n[0]],u=o.length;i<r;++i){e=o,o=t[n[i]];for(var a=0;a<u;++a)o[a][1]+=o[a][0]=isNaN(e[a][1])?e[a][0]:e[a][1]}},fh=function(t){for(var n=t.length,e=new Array(n);--n>=0;)e[n]=n;return e},lh=function(){function t(t){var o,u,a=n.apply(this,arguments),c=t.length,s=a.length,f=new Array(s);for(o=0;o<s;++o){for(var l,h=a[o],p=f[o]=new Array(c),d=0;d<c;++d)p[d]=l=[0,+i(t[d],h,d,t)],l.data=t[d];p.key=h}for(o=0,u=e(f);o<s;++o)f[u[o]].index=o;return r(f,u),f}var n=pl([]),e=fh,r=sh,i=Ot;return t.keys=function(e){return arguments.length?(n="function"==typeof e?e:pl(ch.call(e)),t):n},t.value=function(n){return arguments.length?(i="function"==typeof n?n:pl(+n),t):i},t.order=function(n){return arguments.length?(e=null==n?fh:"function"==typeof n?n:pl(ch.call(n)),t):e},t.offset=function(n){return arguments.length?(r=null==n?sh:n,t):r},t},hh=function(t,n){if((r=t.length)>0){for(var e,r,i,o=0,u=t[0].length;o<u;++o){for(i=e=0;e<r;++e)i+=t[e][o][1]||0;if(i)for(e=0;e<r;++e)t[e][o][1]/=i}sh(t,n)}},ph=function(t,n){if((e=t.length)>0){for(var e,r=0,i=t[n[0]],o=i.length;r<o;++r){for(var u=0,a=0;u<e;++u)a+=t[u][r][1]||0;i[r][1]+=i[r][0]=-a/2}sh(t,n)}},dh=function(t,n){if((i=t.length)>0&&(r=(e=t[n[0]]).length)>0){for(var e,r,i,o=0,u=1;u<r;++u){for(var a=0,c=0,s=0;a<i;++a){for(var f=t[n[a]],l=f[u][1]||0,h=f[u-1][1]||0,p=(l-h)/2,d=0;d<a;++d){var v=t[n[d]],_=v[u][1]||0,y=v[u-1][1]||0;p+=_-y}c+=l,s+=p*l}e[u-1][1]+=e[u-1][0]=o,c&&(o-=s/c)}e[u-1][1]+=e[u-1][0]=o,sh(t,n)}},vh=function(t){var n=t.map(Ft);return fh(t).sort(function(t,e){return n[t]-n[e]})},_h=function(t){return vh(t).reverse()},yh=function(t){var n,e,r=t.length,i=t.map(Ft),o=fh(t).sort(function(t,n){return i[n]-i[t]}),u=0,a=0,c=[],s=[];for(n=0;n<r;++n)e=o[n],u<a?(u+=i[e],c.push(e)):(a+=i[e],s.push(e));return s.reverse().concat(c)},gh=function(t){return fh(t).reverse()},mh=function(t,n,e){t.prototype=n.prototype=e,e.constructor=t},xh=.7,bh=1/xh,wh=/^#([0-9a-f]{3})$/,Mh=/^#([0-9a-f]{6})$/,Th=/^rgb\(\s*([-+]?\d+)\s*,\s*([-+]?\d+)\s*,\s*([-+]?\d+)\s*\)$/,Nh=/^rgb\(\s*([-+]?\d+(?:\.\d+)?)%\s*,\s*([-+]?\d+(?:\.\d+)?)%\s*,\s*([-+]?\d+(?:\.\d+)?)%\s*\)$/,kh=/^rgba\(\s*([-+]?\d+)\s*,\s*([-+]?\d+)\s*,\s*([-+]?\d+)\s*,\s*([-+]?\d+(?:\.\d+)?)\s*\)$/,Sh=/^rgba\(\s*([-+]?\d+(?:\.\d+)?)%\s*,\s*([-+]?\d+(?:\.\d+)?)%\s*,\s*([-+]?\d+(?:\.\d+)?)%\s*,\s*([-+]?\d+(?:\.\d+)?)\s*\)$/,Ah=/^hsl\(\s*([-+]?\d+(?:\.\d+)?)\s*,\s*([-+]?\d+(?:\.\d+)?)%\s*,\s*([-+]?\d+(?:\.\d+)?)%\s*\)$/,Eh=/^hsla\(\s*([-+]?\d+(?:\.\d+)?)\s*,\s*([-+]?\d+(?:\.\d+)?)%\s*,\s*([-+]?\d+(?:\.\d+)?)%\s*,\s*([-+]?\d+(?:\.\d+)?)\s*\)$/,Ch={aliceblue:15792383,antiquewhite:16444375,aqua:65535,aquamarine:8388564,azure:15794175,beige:16119260,bisque:16770244,black:0,blanchedalmond:16772045,blue:255,blueviolet:9055202,brown:10824234,burlywood:14596231,cadetblue:6266528,chartreuse:8388352,chocolate:13789470,coral:16744272,cornflowerblue:6591981,cornsilk:16775388,crimson:14423100,cyan:65535,darkblue:139,darkcyan:35723,darkgoldenrod:12092939,darkgray:11119017,darkgreen:25600,darkgrey:11119017,darkkhaki:12433259,darkmagenta:9109643,darkolivegreen:5597999,darkorange:16747520,darkorchid:10040012,darkred:9109504,darksalmon:15308410,darkseagreen:9419919,darkslateblue:4734347,darkslategray:3100495,darkslategrey:3100495,darkturquoise:52945,darkviolet:9699539,deeppink:16716947,deepskyblue:49151,dimgray:6908265,dimgrey:6908265,dodgerblue:2003199,firebrick:11674146,floralwhite:16775920,forestgreen:2263842,fuchsia:16711935,gainsboro:14474460,ghostwhite:16316671,gold:16766720,goldenrod:14329120,gray:8421504,green:32768,greenyellow:11403055,grey:8421504,honeydew:15794160,hotpink:16738740,indianred:13458524,indigo:4915330,ivory:16777200,khaki:15787660,lavender:15132410,lavenderblush:16773365,lawngreen:8190976,lemonchiffon:16775885,lightblue:11393254,lightcoral:15761536,lightcyan:14745599,lightgoldenrodyellow:16448210,lightgray:13882323,lightgreen:9498256,lightgrey:13882323,lightpink:16758465,lightsalmon:16752762,lightseagreen:2142890,lightskyblue:8900346,lightslategray:7833753,lightslategrey:7833753,lightsteelblue:11584734,lightyellow:16777184,lime:65280,limegreen:3329330,linen:16445670,magenta:16711935,maroon:8388608,mediumaquamarine:6737322,mediumblue:205,mediumorchid:12211667,mediumpurple:9662683,mediumseagreen:3978097,mediumslateblue:8087790,mediumspringgreen:64154,mediumturquoise:4772300,mediumvioletred:13047173,midnightblue:1644912,mintcream:16121850,mistyrose:16770273,moccasin:16770229,navajowhite:16768685,navy:128,oldlace:16643558,olive:8421376,olivedrab:7048739,orange:16753920,orangered:16729344,orchid:14315734,palegoldenrod:15657130,palegreen:10025880,paleturquoise:11529966,palevioletred:14381203,papayawhip:16773077,peachpuff:16767673,peru:13468991,pink:16761035,plum:14524637,powderblue:11591910,purple:8388736,rebeccapurple:6697881,red:16711680,rosybrown:12357519,royalblue:4286945,saddlebrown:9127187,salmon:16416882,sandybrown:16032864,seagreen:3050327,seashell:16774638,sienna:10506797,silver:12632256,skyblue:8900331,slateblue:6970061,slategray:7372944,slategrey:7372944,snow:16775930,springgreen:65407,steelblue:4620980,tan:13808780,teal:32896,thistle:14204888,tomato:16737095,turquoise:4251856,violet:15631086,wheat:16113331,white:16777215,whitesmoke:16119285,yellow:16776960,yellowgreen:10145074};mh(Yt,Bt,{displayable:function(){return this.rgb().displayable()},toString:function(){return this.rgb()+""}}),mh(Wt,Vt,It(Yt,{brighter:function(t){return t=null==t?bh:Math.pow(bh,t),new Wt(this.r*t,this.g*t,this.b*t,this.opacity)},darker:function(t){return t=null==t?xh:Math.pow(xh,t),new Wt(this.r*t,this.g*t,this.b*t,this.opacity)},rgb:function(){return this},displayable:function(){return 0<=this.r&&this.r<=255&&0<=this.g&&this.g<=255&&0<=this.b&&this.b<=255&&0<=this.opacity&&this.opacity<=1},toString:function(){var t=this.opacity;return t=isNaN(t)?1:Math.max(0,Math.min(1,t)),(1===t?"rgb(":"rgba(")+Math.max(0,Math.min(255,Math.round(this.r)||0))+", "+Math.max(0,Math.min(255,Math.round(this.g)||0))+", "+Math.max(0,Math.min(255,Math.round(this.b)||0))+(1===t?")":", "+t+")")}})),mh(Jt,Gt,It(Yt,{brighter:function(t){return t=null==t?bh:Math.pow(bh,t),new Jt(this.h,this.s,this.l*t,this.opacity)},darker:function(t){return t=null==t?xh:Math.pow(xh,t),new Jt(this.h,this.s,this.l*t,this.opacity)},rgb:function(){var t=this.h%360+360*(this.h<0),n=isNaN(t)||isNaN(this.s)?0:this.s,e=this.l,r=e+(e<.5?e:1-e)*n,i=2*e-r;return new Wt(Qt(t>=240?t-240:t+120,i,r),Qt(t,i,r),Qt(t<120?t+240:t-120,i,r),this.opacity)},displayable:function(){return(0<=this.s&&this.s<=1||isNaN(this.s))&&0<=this.l&&this.l<=1&&0<=this.opacity&&this.opacity<=1}}));var zh=Math.PI/180,Ph=180/Math.PI,qh=18,Lh=.95047,Rh=1,Uh=1.08883,Dh=4/29,Oh=6/29,Fh=3*Oh*Oh,Ih=Oh*Oh*Oh;mh(nn,tn,It(Yt,{brighter:function(t){return new nn(this.l+qh*(null==t?1:t),this.a,this.b,this.opacity)},darker:function(t){return new nn(this.l-qh*(null==t?1:t),this.a,this.b,this.opacity)},rgb:function(){var t=(this.l+16)/116,n=isNaN(this.a)?t:t+this.a/500,e=isNaN(this.b)?t:t-this.b/200;return t=Rh*rn(t),n=Lh*rn(n),e=Uh*rn(e),new Wt(on(3.2404542*n-1.5371385*t-.4985314*e),on(-.969266*n+1.8760108*t+.041556*e),on(.0556434*n-.2040259*t+1.0572252*e),this.opacity)}})),mh(sn,cn,It(Yt,{brighter:function(t){return new sn(this.h,this.c,this.l+qh*(null==t?1:t),this.opacity)},darker:function(t){return new sn(this.h,this.c,this.l-qh*(null==t?1:t),this.opacity)},rgb:function(){return Kt(this).rgb()}}));var Yh=-.14861,Bh=1.78277,jh=-.29227,Hh=-.90649,Xh=1.97294,Vh=Xh*Hh,Wh=Xh*Bh,$h=Bh*jh-Hh*Yh;mh(hn,ln,It(Yt,{brighter:function(t){return t=null==t?bh:Math.pow(bh,t),new hn(this.h,this.s,this.l*t,this.opacity)},darker:function(t){return t=null==t?xh:Math.pow(xh,t),new hn(this.h,this.s,this.l*t,this.opacity)},rgb:function(){var t=isNaN(this.h)?0:(this.h+120)*zh,n=+this.l,e=isNaN(this.s)?0:this.s*n*(1-n),r=Math.cos(t),i=Math.sin(t);return new Wt(255*(n+e*(Yh*r+Bh*i)),255*(n+e*(jh*r+Hh*i)),255*(n+e*(Xh*r)),this.opacity)}}));var Zh,Gh,Jh,Qh,Kh=function(t){var n=t.length-1;return function(e){var r=e<=0?e=0:e>=1?(e=1,n-1):Math.floor(e*n),i=t[r],o=t[r+1],u=r>0?t[r-1]:2*i-o,a=r<n-1?t[r+2]:2*o-i;return pn((e-r/n)*n,u,i,o,a)}},tp=function(t){var n=t.length;return function(e){var r=Math.floor(((e%=1)<0?++e:e)*n),i=t[(r+n-1)%n],o=t[r%n],u=t[(r+1)%n],a=t[(r+2)%n];return pn((e-r/n)*n,i,o,u,a)}},np=function(t){return function(){return t}},ep=function t(n){function e(t,n){var e=r((t=Vt(t)).r,(n=Vt(n)).r),i=r(t.g,n.g),o=r(t.b,n.b),u=r(t.opacity,n.opacity);return function(n){return t.r=e(n),t.g=i(n),t.b=o(n),t.opacity=u(n),t+""}}var r=yn(n);return e.gamma=t,e}(1),rp=mn(Kh),ip=mn(tp),op=function(t,n){var e,r=n?n.length:0,i=t?Math.min(r,t.length):0,o=new Array(r),u=new Array(r);for(e=0;e<i;++e)o[e]=hp(t[e],n[e]);for(;e<r;++e)u[e]=n[e];return function(t){for(e=0;e<i;++e)u[e]=o[e](t);return u}},up=function(t,n){var e=new Date;return t=+t,n-=t,function(r){return e.setTime(t+n*r),e}},ap=function(t,n){return t=+t,n-=t,function(e){return t+n*e}},cp=function(t,n){var e,r={},i={};null!==t&&"object"==typeof t||(t={}),null!==n&&"object"==typeof n||(n={});for(e in n)e in t?r[e]=hp(t[e],n[e]):i[e]=n[e];return function(t){for(e in r)i[e]=r[e](t);return i}},sp=/[-+]?(?:\d+\.?\d*|\.?\d+)(?:[eE][-+]?\d+)?/g,fp=new RegExp(sp.source,"g"),lp=function(t,n){var e,r,i,o=sp.lastIndex=fp.lastIndex=0,u=-1,a=[],c=[];for(t+="",n+="";(e=sp.exec(t))&&(r=fp.exec(n));)(i=r.index)>o&&(i=n.slice(o,i),a[u]?a[u]+=i:a[++u]=i),(e=e[0])===(r=r[0])?a[u]?a[u]+=r:a[++u]=r:(a[++u]=null,c.push({i:u,x:ap(e,r)})),o=fp.lastIndex;return o<n.length&&(i=n.slice(o),a[u]?a[u]+=i:a[++u]=i),a.length<2?c[0]?bn(c[0].x):xn(n):(n=c.length,function(t){for(var e,r=0;r<n;++r)a[(e=c[r]).i]=e.x(t);return a.join("")})},hp=function(t,n){var e,r=typeof n;return null==n||"boolean"===r?np(n):("number"===r?ap:"string"===r?(e=Bt(n))?(n=e,ep):lp:n instanceof Bt?ep:n instanceof Date?up:Array.isArray(n)?op:isNaN(n)?cp:ap)(t,n)},pp=function(t,n){return t=+t,n-=t,function(e){return Math.round(t+n*e)}},dp=180/Math.PI,vp={translateX:0,translateY:0,rotate:0,skewX:0,scaleX:1,scaleY:1},_p=function(t,n,e,r,i,o){var u,a,c;return(u=Math.sqrt(t*t+n*n))&&(t/=u,n/=u),(c=t*e+n*r)&&(e-=t*c,r-=n*c),(a=Math.sqrt(e*e+r*r))&&(e/=a,r/=a,c/=a),t*r<n*e&&(t=-t,n=-n,c=-c,u=-u),{translateX:i,translateY:o,rotate:Math.atan2(n,t)*dp,skewX:Math.atan(c)*dp,scaleX:u,scaleY:a}},yp=Tn(wn,"px, ","px)","deg)"),gp=Tn(Mn,", ",")",")"),mp=Math.SQRT2,xp=2,bp=4,wp=1e-12,Mp=function(t,n){var e,r,i=t[0],o=t[1],u=t[2],a=n[0],c=n[1],s=n[2],f=a-i,l=c-o,h=f*f+l*l;if(h<wp)r=Math.log(s/u)/mp,e=function(t){return[i+t*f,o+t*l,u*Math.exp(mp*t*r)]};else{var p=Math.sqrt(h),d=(s*s-u*u+bp*h)/(2*u*xp*p),v=(s*s-u*u-bp*h)/(2*s*xp*p),_=Math.log(Math.sqrt(d*d+1)-d),y=Math.log(Math.sqrt(v*v+1)-v);r=(y-_)/mp,e=function(t){var n=t*r,e=Nn(_),a=u/(xp*p)*(e*Sn(mp*n+_)-kn(_));return[i+a*f,o+a*l,u*e/Nn(mp*n+_)]}}return e.duration=1e3*r,e},Tp=An(_n),Np=An(gn),kp=Cn(_n),Sp=Cn(gn),Ap=zn(_n),Ep=zn(gn),Cp=function(t,n){for(var e=new Array(n),r=0;r<n;++r)e[r]=t(r/(n-1));return e},zp={value:function(){}};qn.prototype=Pn.prototype={constructor:qn,on:function(t,n){var e,r=this._,i=Ln(t+"",r),o=-1,u=i.length;{if(!(arguments.length<2)){if(null!=n&&"function"!=typeof n)throw new Error("invalid callback: "+n);for(;++o<u;)if(e=(t=i[o]).type)r[e]=Un(r[e],t.name,n);else if(null==n)for(e in r)r[e]=Un(r[e],t.name,null);return this}for(;++o<u;)if((e=(t=i[o]).type)&&(e=Rn(r[e],t.name)))return e}},copy:function(){var t={},n=this._;for(var e in n)t[e]=n[e].slice();return new qn(t)},call:function(t,n){if((e=arguments.length-2)>0)for(var e,r,i=new Array(e),o=0;o<e;++o)i[o]=arguments[o+2];if(!this._.hasOwnProperty(t))throw new Error("unknown type: "+t);for(r=this._[t],o=0,e=r.length;o<e;++o)r[o].value.apply(n,i)},apply:function(t,n,e){if(!this._.hasOwnProperty(t))throw new Error("unknown type: "+t);for(var r=this._[t],i=0,o=r.length;i<o;++i)r[i].value.apply(n,e)}};var Pp,qp,Lp=function(t){function n(t,n){var r,i,o=e(t,function(t,e){return r?r(t,e-1):(i=t,void(r=n?On(t,n):Dn(t)))});return o.columns=i,o}function e(t,n){function e(){if(f>=s)return u;if(i)return i=!1,o;var n,e=f;if(34===t.charCodeAt(e)){for(var r=e;r++<s;)if(34===t.charCodeAt(r)){if(34!==t.charCodeAt(r+1))break;++r}return f=r+2,n=t.charCodeAt(r+1),13===n?(i=!0,10===t.charCodeAt(r+2)&&++f):10===n&&(i=!0),t.slice(e+1,r).replace(/""/g,'"')}for(;f<s;){var a=1;if(n=t.charCodeAt(f++),10===n)i=!0;else if(13===n)i=!0,10===t.charCodeAt(f)&&(++f,++a);else if(n!==c)continue;return t.slice(e,f-a)}return t.slice(e)}for(var r,i,o={},u={},a=[],s=t.length,f=0,l=0;(r=e())!==u;){for(var h=[];r!==o&&r!==u;)h.push(r),r=e();n&&null==(h=n(h,l++))||a.push(h)}return a}function r(n,e){return null==e&&(e=Fn(n)),[e.map(u).join(t)].concat(n.map(function(n){return e.map(function(t){return u(n[t]);
}).join(t)})).join("\n")}function i(t){return t.map(o).join("\n")}function o(n){return n.map(u).join(t)}function u(t){return null==t?"":a.test(t+="")?'"'+t.replace(/\"/g,'""')+'"':t}var a=new RegExp('["'+t+"\n]"),c=t.charCodeAt(0);return{parse:n,parseRows:e,format:r,formatRows:i}},Rp=Lp(","),Up=Rp.parse,Dp=Rp.parseRows,Op=Rp.format,Fp=Rp.formatRows,Ip=Lp("\t"),Yp=Ip.parse,Bp=Ip.parseRows,jp=Ip.format,Hp=Ip.formatRows,Xp=function(t,n){function e(t){var n,e=f.status;if(!e&&Yn(f)||e>=200&&e<300||304===e){if(u)try{n=u.call(r,f)}catch(t){return void c.call("error",r,t)}else n=f;c.call("load",r,n)}else c.call("error",r,t)}var r,i,u,a,c=Pn("beforesend","progress","load","error"),s=o(),f=new XMLHttpRequest,l=null,h=null,p=0;if("undefined"==typeof XDomainRequest||"withCredentials"in f||!/^(http(s)?:)?\/\//.test(t)||(f=new XDomainRequest),"onload"in f?f.onload=f.onerror=f.ontimeout=e:f.onreadystatechange=function(t){f.readyState>3&&e(t)},f.onprogress=function(t){c.call("progress",r,t)},r={header:function(t,n){return t=(t+"").toLowerCase(),arguments.length<2?s.get(t):(null==n?s.remove(t):s.set(t,n+""),r)},mimeType:function(t){return arguments.length?(i=null==t?null:t+"",r):i},responseType:function(t){return arguments.length?(a=t,r):a},timeout:function(t){return arguments.length?(p=+t,r):p},user:function(t){return arguments.length<1?l:(l=null==t?null:t+"",r)},password:function(t){return arguments.length<1?h:(h=null==t?null:t+"",r)},response:function(t){return u=t,r},get:function(t,n){return r.send("GET",t,n)},post:function(t,n){return r.send("POST",t,n)},send:function(n,e,o){return f.open(n,t,!0,l,h),null==i||s.has("accept")||s.set("accept",i+",*/*"),f.setRequestHeader&&s.each(function(t,n){f.setRequestHeader(n,t)}),null!=i&&f.overrideMimeType&&f.overrideMimeType(i),null!=a&&(f.responseType=a),p>0&&(f.timeout=p),null==o&&"function"==typeof e&&(o=e,e=null),null!=o&&1===o.length&&(o=In(o)),null!=o&&r.on("error",o).on("load",function(t){o(null,t)}),c.call("beforesend",r,f),f.send(null==e?null:e),r},abort:function(){return f.abort(),r},on:function(){var t=c.on.apply(c,arguments);return t===c?r:t}},null!=n){if("function"!=typeof n)throw new Error("invalid callback: "+n);return r.get(n)}return r},Vp=function(t,n){return function(e,r){var i=Xp(e).mimeType(t).response(n);if(null!=r){if("function"!=typeof r)throw new Error("invalid callback: "+r);return i.get(r)}return i}},Wp=Vp("text/html",function(t){return document.createRange().createContextualFragment(t.responseText)}),$p=Vp("application/json",function(t){return JSON.parse(t.responseText)}),Zp=Vp("text/plain",function(t){return t.responseText}),Gp=Vp("application/xml",function(t){var n=t.responseXML;if(!n)throw new Error("parse error");return n}),Jp=function(t,n){return function(e,r,i){arguments.length<3&&(i=r,r=null);var o=Xp(e).mimeType(t);return o.row=function(t){return arguments.length?o.response(Bn(n,r=t)):r},o.row(r),i?o.get(i):o}},Qp=Jp("text/csv",Up),Kp=Jp("text/tab-separated-values",Yp),td=0,nd=0,ed=0,rd=1e3,id=0,od=0,ud=0,ad="object"==typeof performance&&performance.now?performance:Date,cd="function"==typeof requestAnimationFrame?requestAnimationFrame:function(t){setTimeout(t,17)};Xn.prototype=Vn.prototype={constructor:Xn,restart:function(t,n,e){if("function"!=typeof t)throw new TypeError("callback is not a function");e=(null==e?jn():+e)+(null==n?0:+n),this._next||qp===this||(qp?qp._next=this:Pp=this,qp=this),this._call=t,this._time=e,Jn()},stop:function(){this._call&&(this._call=null,this._time=1/0,Jn())}};var sd=function(t,n,e){var r=new Xn;return n=null==n?0:+n,r.restart(function(e){r.stop(),t(e+n)},n,e),r},fd=function(t,n,e){var r=new Xn,i=n;return null==n?(r.restart(t,n,e),r):(n=+n,e=null==e?jn():+e,r.restart(function o(u){u+=i,r.restart(o,i+=n,e),t(u)},n,e),r)},ld=new Date,hd=new Date,pd=Qn(function(){},function(t,n){t.setTime(+t+n)},function(t,n){return n-t});pd.every=function(t){return t=Math.floor(t),isFinite(t)&&t>0?t>1?Qn(function(n){n.setTime(Math.floor(n/t)*t)},function(n,e){n.setTime(+n+e*t)},function(n,e){return(e-n)/t}):pd:null};var dd=pd.range,vd=1e3,_d=6e4,yd=36e5,gd=864e5,md=6048e5,xd=Qn(function(t){t.setTime(Math.floor(t/vd)*vd)},function(t,n){t.setTime(+t+n*vd)},function(t,n){return(n-t)/vd},function(t){return t.getUTCSeconds()}),bd=xd.range,wd=Qn(function(t){t.setTime(Math.floor(t/_d)*_d)},function(t,n){t.setTime(+t+n*_d)},function(t,n){return(n-t)/_d},function(t){return t.getMinutes()}),Md=wd.range,Td=Qn(function(t){var n=t.getTimezoneOffset()*_d%yd;n<0&&(n+=yd),t.setTime(Math.floor((+t-n)/yd)*yd+n)},function(t,n){t.setTime(+t+n*yd)},function(t,n){return(n-t)/yd},function(t){return t.getHours()}),Nd=Td.range,kd=Qn(function(t){t.setHours(0,0,0,0)},function(t,n){t.setDate(t.getDate()+n)},function(t,n){return(n-t-(n.getTimezoneOffset()-t.getTimezoneOffset())*_d)/gd},function(t){return t.getDate()-1}),Sd=kd.range,Ad=Kn(0),Ed=Kn(1),Cd=Kn(2),zd=Kn(3),Pd=Kn(4),qd=Kn(5),Ld=Kn(6),Rd=Ad.range,Ud=Ed.range,Dd=Cd.range,Od=zd.range,Fd=Pd.range,Id=qd.range,Yd=Ld.range,Bd=Qn(function(t){t.setDate(1),t.setHours(0,0,0,0)},function(t,n){t.setMonth(t.getMonth()+n)},function(t,n){return n.getMonth()-t.getMonth()+12*(n.getFullYear()-t.getFullYear())},function(t){return t.getMonth()}),jd=Bd.range,Hd=Qn(function(t){t.setMonth(0,1),t.setHours(0,0,0,0)},function(t,n){t.setFullYear(t.getFullYear()+n)},function(t,n){return n.getFullYear()-t.getFullYear()},function(t){return t.getFullYear()});Hd.every=function(t){return isFinite(t=Math.floor(t))&&t>0?Qn(function(n){n.setFullYear(Math.floor(n.getFullYear()/t)*t),n.setMonth(0,1),n.setHours(0,0,0,0)},function(n,e){n.setFullYear(n.getFullYear()+e*t)}):null};var Xd=Hd.range,Vd=Qn(function(t){t.setUTCSeconds(0,0)},function(t,n){t.setTime(+t+n*_d)},function(t,n){return(n-t)/_d},function(t){return t.getUTCMinutes()}),Wd=Vd.range,$d=Qn(function(t){t.setUTCMinutes(0,0,0)},function(t,n){t.setTime(+t+n*yd)},function(t,n){return(n-t)/yd},function(t){return t.getUTCHours()}),Zd=$d.range,Gd=Qn(function(t){t.setUTCHours(0,0,0,0)},function(t,n){t.setUTCDate(t.getUTCDate()+n)},function(t,n){return(n-t)/gd},function(t){return t.getUTCDate()-1}),Jd=Gd.range,Qd=te(0),Kd=te(1),tv=te(2),nv=te(3),ev=te(4),rv=te(5),iv=te(6),ov=Qd.range,uv=Kd.range,av=tv.range,cv=nv.range,sv=ev.range,fv=rv.range,lv=iv.range,hv=Qn(function(t){t.setUTCDate(1),t.setUTCHours(0,0,0,0)},function(t,n){t.setUTCMonth(t.getUTCMonth()+n)},function(t,n){return n.getUTCMonth()-t.getUTCMonth()+12*(n.getUTCFullYear()-t.getUTCFullYear())},function(t){return t.getUTCMonth()}),pv=hv.range,dv=Qn(function(t){t.setUTCMonth(0,1),t.setUTCHours(0,0,0,0)},function(t,n){t.setUTCFullYear(t.getUTCFullYear()+n)},function(t,n){return n.getUTCFullYear()-t.getUTCFullYear()},function(t){return t.getUTCFullYear()});dv.every=function(t){return isFinite(t=Math.floor(t))&&t>0?Qn(function(n){n.setUTCFullYear(Math.floor(n.getUTCFullYear()/t)*t),n.setUTCMonth(0,1),n.setUTCHours(0,0,0,0)},function(n,e){n.setUTCFullYear(n.getUTCFullYear()+e*t)}):null};var vv,_v=dv.range,yv=function(t,n){if((e=(t=n?t.toExponential(n-1):t.toExponential()).indexOf("e"))<0)return null;var e,r=t.slice(0,e);return[r.length>1?r[0]+r.slice(2):r,+t.slice(e+1)]},gv=function(t){return t=yv(Math.abs(t)),t?t[1]:NaN},mv=function(t,n){return function(e,r){for(var i=e.length,o=[],u=0,a=t[0],c=0;i>0&&a>0&&(c+a+1>r&&(a=Math.max(1,r-c)),o.push(e.substring(i-=a,i+a)),!((c+=a+1)>r));)a=t[u=(u+1)%t.length];return o.reverse().join(n)}},xv=function(t,n){t=t.toPrecision(n);t:for(var e,r=t.length,i=1,o=-1;i<r;++i)switch(t[i]){case".":o=e=i;break;case"0":0===o&&(o=i),e=i;break;case"e":break t;default:o>0&&(o=0)}return o>0?t.slice(0,o)+t.slice(e+1):t},bv=function(t,n){var e=yv(t,n);if(!e)return t+"";var r=e[0],i=e[1],o=i-(vv=3*Math.max(-8,Math.min(8,Math.floor(i/3))))+1,u=r.length;return o===u?r:o>u?r+new Array(o-u+1).join("0"):o>0?r.slice(0,o)+"."+r.slice(o):"0."+new Array(1-o).join("0")+yv(t,Math.max(0,n+o-1))[0]},wv=function(t,n){var e=yv(t,n);if(!e)return t+"";var r=e[0],i=e[1];return i<0?"0."+new Array(-i).join("0")+r:r.length>i+1?r.slice(0,i+1)+"."+r.slice(i+1):r+new Array(i-r.length+2).join("0")},Mv={"":xv,"%":function(t,n){return(100*t).toFixed(n)},b:function(t){return Math.round(t).toString(2)},c:function(t){return t+""},d:function(t){return Math.round(t).toString(10)},e:function(t,n){return t.toExponential(n)},f:function(t,n){return t.toFixed(n)},g:function(t,n){return t.toPrecision(n)},o:function(t){return Math.round(t).toString(8)},p:function(t,n){return wv(100*t,n)},r:wv,s:bv,X:function(t){return Math.round(t).toString(16).toUpperCase()},x:function(t){return Math.round(t).toString(16)}},Tv=/^(?:(.)?([<>=^]))?([+\-\( ])?([$#])?(0)?(\d+)?(,)?(\.\d+)?([a-z%])?$/i,Nv=function(t){return new ne(t)};ne.prototype.toString=function(){return this.fill+this.align+this.sign+this.symbol+(this.zero?"0":"")+(null==this.width?"":Math.max(1,0|this.width))+(this.comma?",":"")+(null==this.precision?"":"."+Math.max(0,0|this.precision))+this.type};var kv,Sv=["y","z","a","f","p","n","µ","m","","k","M","G","T","P","E","Z","Y"],Av=function(t){function n(t){function n(t){var n,i,c,g=d,m=v;if("c"===p)m=_(t)+m,t="";else{t=+t;var x=(t<0||1/t<0)&&(t*=-1,!0);if(t=_(t,h),x)for(n=-1,i=t.length,x=!1;++n<i;)if(c=t.charCodeAt(n),48<c&&c<58||"x"===p&&96<c&&c<103||"X"===p&&64<c&&c<71){x=!0;break}if(g=(x?"("===a?a:"-":"-"===a||"("===a?"":a)+g,m=m+("s"===p?Sv[8+vv/3]:"")+(x&&"("===a?")":""),y)for(n=-1,i=t.length;++n<i;)if(c=t.charCodeAt(n),48>c||c>57){m=(46===c?o+t.slice(n+1):t.slice(n))+m,t=t.slice(0,n);break}}l&&!s&&(t=r(t,1/0));var b=g.length+t.length+m.length,w=b<f?new Array(f-b+1).join(e):"";switch(l&&s&&(t=r(w+t,w.length?f-m.length:1/0),w=""),u){case"<":return g+t+m+w;case"=":return g+w+t+m;case"^":return w.slice(0,b=w.length>>1)+g+t+m+w.slice(b)}return w+g+t+m}t=Nv(t);var e=t.fill,u=t.align,a=t.sign,c=t.symbol,s=t.zero,f=t.width,l=t.comma,h=t.precision,p=t.type,d="$"===c?i[0]:"#"===c&&/[boxX]/.test(p)?"0"+p.toLowerCase():"",v="$"===c?i[1]:/[%p]/.test(p)?"%":"",_=Mv[p],y=!p||/[defgprs%]/.test(p);return h=null==h?p?6:12:/[gprs]/.test(p)?Math.max(1,Math.min(21,h)):Math.max(0,Math.min(20,h)),n.toString=function(){return t+""},n}function e(t,e){var r=n((t=Nv(t),t.type="f",t)),i=3*Math.max(-8,Math.min(8,Math.floor(gv(e)/3))),o=Math.pow(10,-i),u=Sv[8+i/3];return function(t){return r(o*t)+u}}var r=t.grouping&&t.thousands?mv(t.grouping,t.thousands):ee,i=t.currency,o=t.decimal;return{format:n,formatPrefix:e}};re({decimal:".",thousands:",",grouping:[3],currency:["$",""]});var Ev,Cv=function(t){return Math.max(0,-gv(Math.abs(t)))},zv=function(t,n){return Math.max(0,3*Math.max(-8,Math.min(8,Math.floor(gv(n)/3)))-gv(Math.abs(t)))},Pv=function(t,n){return t=Math.abs(t),n=Math.abs(n)-t,Math.max(0,gv(n)-gv(t))+1},qv={"-":"",_:" ",0:"0"},Lv=/^\s*\d+/,Rv=/^%/,Uv=/[\\\^\$\*\+\?\|\[\]\(\)\.\{\}]/g;nr({dateTime:"%x, %X",date:"%-m/%-d/%Y",time:"%-I:%M:%S %p",periods:["AM","PM"],days:["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"],shortDays:["Sun","Mon","Tue","Wed","Thu","Fri","Sat"],months:["January","February","March","April","May","June","July","August","September","October","November","December"],shortMonths:["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"]});var Dv="%Y-%m-%dT%H:%M:%S.%LZ",Ov=Date.prototype.toISOString?er:t.utcFormat(Dv),Fv=+new Date("2000-01-01T00:00:00.000Z")?rr:t.utcParse(Dv),Iv=Array.prototype,Yv=Iv.map,Bv=Iv.slice,jv={name:"implicit"},Hv=function(t){return function(){return t}},Xv=function(t){return+t},Vv=[0,1],Wv=function(n,r,i){var o,u=n[0],a=n[n.length-1],c=e(u,a,null==r?10:r);switch(i=Nv(null==i?",f":i),i.type){case"s":var s=Math.max(Math.abs(u),Math.abs(a));return null!=i.precision||isNaN(o=zv(c,s))||(i.precision=o),t.formatPrefix(i,s);case"":case"e":case"g":case"p":case"r":null!=i.precision||isNaN(o=Pv(c,Math.max(Math.abs(u),Math.abs(a))))||(i.precision=o-("e"===i.type));break;case"f":case"%":null!=i.precision||isNaN(o=Cv(c))||(i.precision=o-2*("%"===i.type))}return t.format(i)},$v=function(t,n){t=t.slice();var e,r=0,i=t.length-1,o=t[r],u=t[i];return u<o&&(e=r,r=i,i=e,e=o,o=u,u=e),t[r]=n.floor(o),t[i]=n.ceil(u),t},Zv=1e3,Gv=60*Zv,Jv=60*Gv,Qv=24*Jv,Kv=7*Qv,t_=30*Qv,n_=365*Qv,e_=function(){return qr(Hd,Bd,Ad,kd,Td,wd,xd,pd,t.timeFormat).domain([new Date(2e3,0,1),new Date(2e3,0,2)])},r_=function(){return qr(dv,hv,Qd,Gd,$d,Vd,xd,pd,t.utcFormat).domain([Date.UTC(2e3,0,1),Date.UTC(2e3,0,2)])},i_=function(t){return t.match(/.{6}/g).map(function(t){return"#"+t})},o_=i_("1f77b4ff7f0e2ca02cd627289467bd8c564be377c27f7f7fbcbd2217becf"),u_=i_("393b795254a36b6ecf9c9ede6379398ca252b5cf6bcedb9c8c6d31bd9e39e7ba52e7cb94843c39ad494ad6616be7969c7b4173a55194ce6dbdde9ed6"),a_=i_("3182bd6baed69ecae1c6dbefe6550dfd8d3cfdae6bfdd0a231a35474c476a1d99bc7e9c0756bb19e9ac8bcbddcdadaeb636363969696bdbdbdd9d9d9"),c_=i_("1f77b4aec7e8ff7f0effbb782ca02c98df8ad62728ff98969467bdc5b0d58c564bc49c94e377c2f7b6d27f7f7fc7c7c7bcbd22dbdb8d17becf9edae5"),s_=Ep(ln(300,.5,0),ln(-240,.5,1)),f_=Ep(ln(-100,.75,.35),ln(80,1.5,.8)),l_=Ep(ln(260,.75,.35),ln(80,1.5,.8)),h_=ln(),p_=function(t){(t<0||t>1)&&(t-=Math.floor(t));var n=Math.abs(t-.5);return h_.h=360*t-100,h_.s=1.5-1.5*n,h_.l=.8-.9*n,h_+""},d_=Lr(i_("44015444025645045745055946075a46085c460a5d460b5e470d60470e6147106347116447136548146748166848176948186a481a6c481b6d481c6e481d6f481f70482071482173482374482475482576482677482878482979472a7a472c7a472d7b472e7c472f7d46307e46327e46337f463480453581453781453882443983443a83443b84433d84433e85423f854240864241864142874144874045884046883f47883f48893e49893e4a893e4c8a3d4d8a3d4e8a3c4f8a3c508b3b518b3b528b3a538b3a548c39558c39568c38588c38598c375a8c375b8d365c8d365d8d355e8d355f8d34608d34618d33628d33638d32648e32658e31668e31678e31688e30698e306a8e2f6b8e2f6c8e2e6d8e2e6e8e2e6f8e2d708e2d718e2c718e2c728e2c738e2b748e2b758e2a768e2a778e2a788e29798e297a8e297b8e287c8e287d8e277e8e277f8e27808e26818e26828e26828e25838e25848e25858e24868e24878e23888e23898e238a8d228b8d228c8d228d8d218e8d218f8d21908d21918c20928c20928c20938c1f948c1f958b1f968b1f978b1f988b1f998a1f9a8a1e9b8a1e9c891e9d891f9e891f9f881fa0881fa1881fa1871fa28720a38620a48621a58521a68522a78522a88423a98324aa8325ab8225ac8226ad8127ad8128ae8029af7f2ab07f2cb17e2db27d2eb37c2fb47c31b57b32b67a34b67935b77937b87838b9773aba763bbb753dbc743fbc7340bd7242be7144bf7046c06f48c16e4ac16d4cc26c4ec36b50c46a52c56954c56856c66758c7655ac8645cc8635ec96260ca6063cb5f65cb5e67cc5c69cd5b6ccd5a6ece5870cf5773d05675d05477d1537ad1517cd2507fd34e81d34d84d44b86d54989d5488bd6468ed64590d74393d74195d84098d83e9bd93c9dd93ba0da39a2da37a5db36a8db34aadc32addc30b0dd2fb2dd2db5de2bb8de29bade28bddf26c0df25c2df23c5e021c8e020cae11fcde11dd0e11cd2e21bd5e21ad8e219dae319dde318dfe318e2e418e5e419e7e419eae51aece51befe51cf1e51df4e61ef6e620f8e621fbe723fde725")),v_=Lr(i_("00000401000501010601010802010902020b02020d03030f03031204041405041606051806051a07061c08071e0907200a08220b09240c09260d0a290e0b2b100b2d110c2f120d31130d34140e36150e38160f3b180f3d19103f1a10421c10441d11471e114920114b21114e22115024125325125527125829115a2a115c2c115f2d11612f116331116533106734106936106b38106c390f6e3b0f703d0f713f0f72400f74420f75440f764510774710784910784a10794c117a4e117b4f127b51127c52137c54137d56147d57157e59157e5a167e5c167f5d177f5f187f601880621980641a80651a80671b80681c816a1c816b1d816d1d816e1e81701f81721f817320817521817621817822817922827b23827c23827e24828025828125818326818426818627818827818928818b29818c29818e2a81902a81912b81932b80942c80962c80982d80992d809b2e7f9c2e7f9e2f7fa02f7fa1307ea3307ea5317ea6317da8327daa337dab337cad347cae347bb0357bb2357bb3367ab5367ab73779b83779ba3878bc3978bd3977bf3a77c03a76c23b75c43c75c53c74c73d73c83e73ca3e72cc3f71cd4071cf4070d0416fd2426fd3436ed5446dd6456cd8456cd9466bdb476adc4869de4968df4a68e04c67e24d66e34e65e44f64e55064e75263e85362e95462ea5661eb5760ec5860ed5a5fee5b5eef5d5ef05f5ef1605df2625df2645cf3655cf4675cf4695cf56b5cf66c5cf66e5cf7705cf7725cf8745cf8765cf9785df9795df97b5dfa7d5efa7f5efa815ffb835ffb8560fb8761fc8961fc8a62fc8c63fc8e64fc9065fd9266fd9467fd9668fd9869fd9a6afd9b6bfe9d6cfe9f6dfea16efea36ffea571fea772fea973feaa74feac76feae77feb078feb27afeb47bfeb67cfeb77efeb97ffebb81febd82febf84fec185fec287fec488fec68afec88cfeca8dfecc8ffecd90fecf92fed194fed395fed597fed799fed89afdda9cfddc9efddea0fde0a1fde2a3fde3a5fde5a7fde7a9fde9aafdebacfcecaefceeb0fcf0b2fcf2b4fcf4b6fcf6b8fcf7b9fcf9bbfcfbbdfcfdbf")),__=Lr(i_("00000401000501010601010802010a02020c02020e03021004031204031405041706041907051b08051d09061f0a07220b07240c08260d08290e092b10092d110a30120a32140b34150b37160b39180c3c190c3e1b0c411c0c431e0c451f0c48210c4a230c4c240c4f260c51280b53290b552b0b572d0b592f0a5b310a5c320a5e340a5f3609613809623909633b09643d09653e0966400a67420a68440a68450a69470b6a490b6a4a0c6b4c0c6b4d0d6c4f0d6c510e6c520e6d540f6d550f6d57106e59106e5a116e5c126e5d126e5f136e61136e62146e64156e65156e67166e69166e6a176e6c186e6d186e6f196e71196e721a6e741a6e751b6e771c6d781c6d7a1d6d7c1d6d7d1e6d7f1e6c801f6c82206c84206b85216b87216b88226a8a226a8c23698d23698f24699025689225689326679526679727669827669a28659b29649d29649f2a63a02a63a22b62a32c61a52c60a62d60a82e5fa92e5eab2f5ead305dae305cb0315bb1325ab3325ab43359b63458b73557b93556ba3655bc3754bd3853bf3952c03a51c13a50c33b4fc43c4ec63d4dc73e4cc83f4bca404acb4149cc4248ce4347cf4446d04545d24644d34743d44842d54a41d74b3fd84c3ed94d3dda4e3cdb503bdd513ade5238df5337e05536e15635e25734e35933e45a31e55c30e65d2fe75e2ee8602de9612bea632aeb6429eb6628ec6726ed6925ee6a24ef6c23ef6e21f06f20f1711ff1731df2741cf3761bf37819f47918f57b17f57d15f67e14f68013f78212f78410f8850ff8870ef8890cf98b0bf98c0af98e09fa9008fa9207fa9407fb9606fb9706fb9906fb9b06fb9d07fc9f07fca108fca309fca50afca60cfca80dfcaa0ffcac11fcae12fcb014fcb216fcb418fbb61afbb81dfbba1ffbbc21fbbe23fac026fac228fac42afac62df9c72ff9c932f9cb35f8cd37f8cf3af7d13df7d340f6d543f6d746f5d949f5db4cf4dd4ff4df53f4e156f3e35af3e55df2e661f2e865f2ea69f1ec6df1ed71f1ef75f1f179f2f27df2f482f3f586f3f68af4f88ef5f992f6fa96f8fb9af9fc9dfafda1fcffa4")),y_=Lr(i_("0d088710078813078916078a19068c1b068d1d068e20068f2206902406912605912805922a05932c05942e05952f059631059733059735049837049938049a3a049a3c049b3e049c3f049c41049d43039e44039e46039f48039f4903a04b03a14c02a14e02a25002a25102a35302a35502a45601a45801a45901a55b01a55c01a65e01a66001a66100a76300a76400a76600a76700a86900a86a00a86c00a86e00a86f00a87100a87201a87401a87501a87701a87801a87a02a87b02a87d03a87e03a88004a88104a78305a78405a78606a68707a68808a68a09a58b0aa58d0ba58e0ca48f0da4910ea3920fa39410a29511a19613a19814a099159f9a169f9c179e9d189d9e199da01a9ca11b9ba21d9aa31e9aa51f99a62098a72197a82296aa2395ab2494ac2694ad2793ae2892b02991b12a90b22b8fb32c8eb42e8db52f8cb6308bb7318ab83289ba3388bb3488bc3587bd3786be3885bf3984c03a83c13b82c23c81c33d80c43e7fc5407ec6417dc7427cc8437bc9447aca457acb4679cc4778cc4977cd4a76ce4b75cf4c74d04d73d14e72d24f71d35171d45270d5536fd5546ed6556dd7566cd8576bd9586ada5a6ada5b69db5c68dc5d67dd5e66de5f65de6164df6263e06363e16462e26561e26660e3685fe4695ee56a5de56b5de66c5ce76e5be76f5ae87059e97158e97257ea7457eb7556eb7655ec7754ed7953ed7a52ee7b51ef7c51ef7e50f07f4ff0804ef1814df1834cf2844bf3854bf3874af48849f48948f58b47f58c46f68d45f68f44f79044f79143f79342f89441f89540f9973ff9983ef99a3efa9b3dfa9c3cfa9e3bfb9f3afba139fba238fca338fca537fca636fca835fca934fdab33fdac33fdae32fdaf31fdb130fdb22ffdb42ffdb52efeb72dfeb82cfeba2cfebb2bfebd2afebe2afec029fdc229fdc328fdc527fdc627fdc827fdca26fdcb26fccd25fcce25fcd025fcd225fbd324fbd524fbd724fad824fada24f9dc24f9dd25f8df25f8e125f7e225f7e425f6e626f6e826f5e926f5eb27f4ed27f3ee27f3f027f2f227f1f426f1f525f0f724f0f921")),g_="http://www.w3.org/1999/xhtml",m_={svg:"http://www.w3.org/2000/svg",xhtml:g_,xlink:"http://www.w3.org/1999/xlink",xml:"http://www.w3.org/XML/1998/namespace",xmlns:"http://www.w3.org/2000/xmlns/"},x_=function(t){var n=t+="",e=n.indexOf(":");return e>=0&&"xmlns"!==(n=t.slice(0,e))&&(t=t.slice(e+1)),m_.hasOwnProperty(n)?{space:m_[n],local:t}:t},b_=function(t){var n=x_(t);return(n.local?Dr:Ur)(n)},w_=0;Fr.prototype=Or.prototype={constructor:Fr,get:function(t){for(var n=this._;!(n in t);)if(!(t=t.parentNode))return;return t[n]},set:function(t,n){return t[this._]=n},remove:function(t){return this._ in t&&delete t[this._]},toString:function(){return this._}};var M_=function(t){return function(){return this.matches(t)}};if("undefined"!=typeof document){var T_=document.documentElement;if(!T_.matches){var N_=T_.webkitMatchesSelector||T_.msMatchesSelector||T_.mozMatchesSelector||T_.oMatchesSelector;M_=function(t){return function(){return N_.call(this,t)}}}}var k_=M_,S_={};if(t.event=null,"undefined"!=typeof document){var A_=document.documentElement;"onmouseenter"in A_||(S_={mouseenter:"mouseover",mouseleave:"mouseout"})}var E_=function(t,n,e){var r,i,o=Br(t+""),u=o.length;{if(!(arguments.length<2)){for(a=n?Hr:jr,null==e&&(e=!1),r=0;r<u;++r)this.each(a(o[r],n,e));return this}var a=this.node().__on;if(a)for(var c,s=0,f=a.length;s<f;++s)for(r=0,c=a[s];r<u;++r)if((i=o[r]).type===c.type&&i.name===c.name)return c.value}},C_=function(){for(var n,e=t.event;n=e.sourceEvent;)e=n;return e},z_=function(t,n){var e=t.ownerSVGElement||t;if(e.createSVGPoint){var r=e.createSVGPoint();return r.x=n.clientX,r.y=n.clientY,r=r.matrixTransform(t.getScreenCTM().inverse()),[r.x,r.y]}var i=t.getBoundingClientRect();return[n.clientX-i.left-t.clientLeft,n.clientY-i.top-t.clientTop]},P_=function(t){var n=C_();return n.changedTouches&&(n=n.changedTouches[0]),z_(t,n)},q_=function(t){return null==t?Vr:function(){return this.querySelector(t)}},L_=function(t){"function"!=typeof t&&(t=q_(t));for(var n=this._groups,e=n.length,r=new Array(e),i=0;i<e;++i)for(var o,u,a=n[i],c=a.length,s=r[i]=new Array(c),f=0;f<c;++f)(o=a[f])&&(u=t.call(o,o.__data__,f,a))&&("__data__"in o&&(u.__data__=o.__data__),s[f]=u);return new zi(r,this._parents)},R_=function(t){return null==t?Wr:function(){return this.querySelectorAll(t)}},U_=function(t){"function"!=typeof t&&(t=R_(t));for(var n=this._groups,e=n.length,r=[],i=[],o=0;o<e;++o)for(var u,a=n[o],c=a.length,s=0;s<c;++s)(u=a[s])&&(r.push(t.call(u,u.__data__,s,a)),i.push(u));return new zi(r,i)},D_=function(t){"function"!=typeof t&&(t=k_(t));for(var n=this._groups,e=n.length,r=new Array(e),i=0;i<e;++i)for(var o,u=n[i],a=u.length,c=r[i]=[],s=0;s<a;++s)(o=u[s])&&t.call(o,o.__data__,s,u)&&c.push(o);return new zi(r,this._parents)},O_=function(t){return new Array(t.length)},F_=function(){return new zi(this._enter||this._groups.map(O_),this._parents)};$r.prototype={constructor:$r,appendChild:function(t){return this._parent.insertBefore(t,this._next)},insertBefore:function(t,n){return this._parent.insertBefore(t,n)},querySelector:function(t){return this._parent.querySelector(t)},querySelectorAll:function(t){return this._parent.querySelectorAll(t)}};var I_=function(t){return function(){return t}},Y_="$",B_=function(t,n){if(!t)return p=new Array(this.size()),s=-1,this.each(function(t){p[++s]=t}),p;var e=n?Gr:Zr,r=this._parents,i=this._groups;"function"!=typeof t&&(t=I_(t));for(var o=i.length,u=new Array(o),a=new Array(o),c=new Array(o),s=0;s<o;++s){var f=r[s],l=i[s],h=l.length,p=t.call(f,f&&f.__data__,s,r),d=p.length,v=a[s]=new Array(d),_=u[s]=new Array(d),y=c[s]=new Array(h);e(f,l,v,_,y,p,n);for(var g,m,x=0,b=0;x<d;++x)if(g=v[x]){for(x>=b&&(b=x+1);!(m=_[b])&&++b<d;);g._next=m||null}}return u=new zi(u,r),u._enter=a,u._exit=c,u},j_=function(){return new zi(this._exit||this._groups.map(O_),this._parents)},H_=function(t){for(var n=this._groups,e=t._groups,r=n.length,i=e.length,o=Math.min(r,i),u=new Array(r),a=0;a<o;++a)for(var c,s=n[a],f=e[a],l=s.length,h=u[a]=new Array(l),p=0;p<l;++p)(c=s[p]||f[p])&&(h[p]=c);for(;a<r;++a)u[a]=n[a];return new zi(u,this._parents)},X_=function(){for(var t=this._groups,n=-1,e=t.length;++n<e;)for(var r,i=t[n],o=i.length-1,u=i[o];--o>=0;)(r=i[o])&&(u&&u!==r.nextSibling&&u.parentNode.insertBefore(r,u),u=r);return this},V_=function(t){function n(n,e){return n&&e?t(n.__data__,e.__data__):!n-!e}t||(t=Jr);for(var e=this._groups,r=e.length,i=new Array(r),o=0;o<r;++o){for(var u,a=e[o],c=a.length,s=i[o]=new Array(c),f=0;f<c;++f)(u=a[f])&&(s[f]=u);s.sort(n)}return new zi(i,this._parents).order()},W_=function(){var t=arguments[0];return arguments[0]=this,t.apply(null,arguments),this},$_=function(){var t=new Array(this.size()),n=-1;return this.each(function(){t[++n]=this}),t},Z_=function(){for(var t=this._groups,n=0,e=t.length;n<e;++n)for(var r=t[n],i=0,o=r.length;i<o;++i){var u=r[i];if(u)return u}return null},G_=function(){var t=0;return this.each(function(){++t}),t},J_=function(){return!this.node()},Q_=function(t){for(var n=this._groups,e=0,r=n.length;e<r;++e)for(var i,o=n[e],u=0,a=o.length;u<a;++u)(i=o[u])&&t.call(i,i.__data__,u,o);return this},K_=function(t,n){var e=x_(t);if(arguments.length<2){var r=this.node();return e.local?r.getAttributeNS(e.space,e.local):r.getAttribute(e)}return this.each((null==n?e.local?Kr:Qr:"function"==typeof n?e.local?ri:ei:e.local?ni:ti)(e,n))},ty=function(t){return t.ownerDocument&&t.ownerDocument.defaultView||t.document&&t||t.defaultView},ny=function(t,n,e){var r;return arguments.length>1?this.each((null==n?ii:"function"==typeof n?ui:oi)(t,n,null==e?"":e)):ty(r=this.node()).getComputedStyle(r,null).getPropertyValue(t)},ey=function(t,n){return arguments.length>1?this.each((null==n?ai:"function"==typeof n?si:ci)(t,n)):this.node()[t]};hi.prototype={add:function(t){var n=this._names.indexOf(t);n<0&&(this._names.push(t),this._node.setAttribute("class",this._names.join(" ")))},remove:function(t){var n=this._names.indexOf(t);n>=0&&(this._names.splice(n,1),this._node.setAttribute("class",this._names.join(" ")))},contains:function(t){return this._names.indexOf(t)>=0}};var ry=function(t,n){var e=fi(t+"");if(arguments.length<2){for(var r=li(this.node()),i=-1,o=e.length;++i<o;)if(!r.contains(e[i]))return!1;return!0}return this.each(("function"==typeof n?yi:n?vi:_i)(e,n))},iy=function(t){return arguments.length?this.each(null==t?gi:("function"==typeof t?xi:mi)(t)):this.node().textContent},oy=function(t){return arguments.length?this.each(null==t?bi:("function"==typeof t?Mi:wi)(t)):this.node().innerHTML},uy=function(){return this.each(Ti)},ay=function(){return this.each(Ni)},cy=function(t){var n="function"==typeof t?t:b_(t);return this.select(function(){return this.appendChild(n.apply(this,arguments))})},sy=function(t,n){var e="function"==typeof t?t:b_(t),r=null==n?ki:"function"==typeof n?n:q_(n);return this.select(function(){return this.insertBefore(e.apply(this,arguments),r.apply(this,arguments)||null)})},fy=function(){return this.each(Si)},ly=function(t){return arguments.length?this.property("__data__",t):this.node().__data__},hy=function(t,n){return this.each(("function"==typeof n?Ci:Ei)(t,n))},py=[null];zi.prototype=Pi.prototype={constructor:zi,select:L_,selectAll:U_,filter:D_,data:B_,enter:F_,exit:j_,merge:H_,order:X_,sort:V_,call:W_,nodes:$_,node:Z_,size:G_,empty:J_,each:Q_,attr:K_,style:ny,property:ey,classed:ry,text:iy,html:oy,raise:uy,lower:ay,append:cy,insert:sy,remove:fy,datum:ly,on:E_,dispatch:hy};var dy=function(t){return"string"==typeof t?new zi([[document.querySelector(t)]],[document.documentElement]):new zi([[t]],py)},vy=function(t){return"string"==typeof t?new zi([document.querySelectorAll(t)],[document.documentElement]):new zi([null==t?[]:t],py)},_y=function(t,n,e){arguments.length<3&&(e=n,n=C_().changedTouches);for(var r,i=0,o=n?n.length:0;i<o;++i)if((r=n[i]).identifier===e)return z_(t,r);return null},yy=function(t,n){null==n&&(n=C_().touches);for(var e=0,r=n?n.length:0,i=new Array(r);e<r;++e)i[e]=z_(t,n[e]);return i},gy=Pn("start","end","interrupt"),my=[],xy=0,by=1,wy=2,My=3,Ty=4,Ny=5,ky=6,Sy=function(t,n,e,r,i,o){var u=t.__transition;if(u){if(e in u)return}else t.__transition={};Ui(t,e,{name:n,index:r,group:i,on:gy,tween:my,time:o.time,delay:o.delay,duration:o.duration,ease:o.ease,timer:null,state:xy})},Ay=function(t,n){var e,r,i,o=t.__transition,u=!0;if(o){n=null==n?null:n+"";for(i in o)(e=o[i]).name===n?(r=e.state===My,e.state=ky,e.timer.stop(),r&&e.on.call("interrupt",t,t.__data__,e.index,e.group),delete o[i]):u=!1;u&&delete t.__transition}},Ey=function(t){return this.each(function(){Ay(this,t)})},Cy=function(t,n){var e=this._id;if(t+="",arguments.length<2){for(var r,i=Ri(this.node(),e).tween,o=0,u=i.length;o<u;++o)if((r=i[o]).name===t)return r.value;return null}return this.each((null==n?Di:Oi)(e,t,n))},zy=function(t,n){var e;return("number"==typeof n?ap:n instanceof Bt?ep:(e=Bt(n))?(n=e,ep):lp)(t,n)},Py=function(t,n){var e=x_(t),r="transform"===e?gp:zy;return this.attrTween(t,"function"==typeof n?(e.local?Xi:Hi)(e,r,Fi(this,"attr."+t,n)):null==n?(e.local?Yi:Ii)(e):(e.local?ji:Bi)(e,r,n))},qy=function(t,n){var e="attr."+t;if(arguments.length<2)return(e=this.tween(e))&&e._value;if(null==n)return this.tween(e,null);if("function"!=typeof n)throw new Error;var r=x_(t);return this.tween(e,(r.local?Vi:Wi)(r,n))},Ly=function(t){var n=this._id;return arguments.length?this.each(("function"==typeof t?$i:Zi)(n,t)):Ri(this.node(),n).delay},Ry=function(t){var n=this._id;return arguments.length?this.each(("function"==typeof t?Gi:Ji)(n,t)):Ri(this.node(),n).duration},Uy=function(t){var n=this._id;return arguments.length?this.each(Qi(n,t)):Ri(this.node(),n).ease},Dy=function(t){"function"!=typeof t&&(t=k_(t));for(var n=this._groups,e=n.length,r=new Array(e),i=0;i<e;++i)for(var o,u=n[i],a=u.length,c=r[i]=[],s=0;s<a;++s)(o=u[s])&&t.call(o,o.__data__,s,u)&&c.push(o);return new so(r,this._parents,this._name,this._id)},Oy=function(t){if(t._id!==this._id)throw new Error;for(var n=this._groups,e=t._groups,r=n.length,i=e.length,o=Math.min(r,i),u=new Array(r),a=0;a<o;++a)for(var c,s=n[a],f=e[a],l=s.length,h=u[a]=new Array(l),p=0;p<l;++p)(c=s[p]||f[p])&&(h[p]=c);for(;a<r;++a)u[a]=n[a];return new so(u,this._parents,this._name,this._id)},Fy=function(t,n){var e=this._id;return arguments.length<2?Ri(this.node(),e).on.on(t):this.each(to(e,t,n))},Iy=function(){return this.on("end.remove",no(this._id))},Yy=function(t){var n=this._name,e=this._id;"function"!=typeof t&&(t=q_(t));for(var r=this._groups,i=r.length,o=new Array(i),u=0;u<i;++u)for(var a,c,s=r[u],f=s.length,l=o[u]=new Array(f),h=0;h<f;++h)(a=s[h])&&(c=t.call(a,a.__data__,h,s))&&("__data__"in a&&(c.__data__=a.__data__),l[h]=c,Sy(l[h],n,e,h,l,Ri(a,e)));return new so(o,this._parents,n,e)},By=function(t){var n=this._name,e=this._id;"function"!=typeof t&&(t=R_(t));for(var r=this._groups,i=r.length,o=[],u=[],a=0;a<i;++a)for(var c,s=r[a],f=s.length,l=0;l<f;++l)if(c=s[l]){for(var h,p=t.call(c,c.__data__,l,s),d=Ri(c,e),v=0,_=p.length;v<_;++v)(h=p[v])&&Sy(h,n,e,v,p,d);o.push(p),u.push(c)}return new so(o,u,n,e)},jy=Pi.prototype.constructor,Hy=function(){return new jy(this._groups,this._parents)},Xy=function(t,n,e){var r="transform"==(t+="")?yp:zy;return null==n?this.styleTween(t,eo(t,r)).on("end.style."+t,ro(t)):this.styleTween(t,"function"==typeof n?oo(t,r,Fi(this,"style."+t,n)):io(t,r,n),e)},Vy=function(t,n,e){var r="style."+(t+="");if(arguments.length<2)return(r=this.tween(r))&&r._value;if(null==n)return this.tween(r,null);if("function"!=typeof n)throw new Error;return this.tween(r,uo(t,n,null==e?"":e))},Wy=function(t){return this.tween("text","function"==typeof t?co(Fi(this,"text",t)):ao(null==t?"":t+""))},$y=function(){for(var t=this._name,n=this._id,e=lo(),r=this._groups,i=r.length,o=0;o<i;++o)for(var u,a=r[o],c=a.length,s=0;s<c;++s)if(u=a[s]){var f=Ri(u,n);Sy(u,t,e,s,a,{time:f.time+f.delay+f.duration,delay:0,duration:f.duration,ease:f.ease})}return new so(r,this._parents,t,e)},Zy=0,Gy=Pi.prototype;so.prototype=fo.prototype={constructor:so,select:Yy,selectAll:By,filter:Dy,merge:Oy,selection:Hy,transition:$y,call:Gy.call,nodes:Gy.nodes,node:Gy.node,size:Gy.size,empty:Gy.empty,each:Gy.each,on:Fy,attr:Py,attrTween:qy,style:Xy,styleTween:Vy,text:Wy,remove:Iy,tween:Cy,delay:Ly,duration:Ry,ease:Uy};var Jy={time:null,delay:0,duration:250,ease:g},Qy=function(t){var n,e;t instanceof so?(n=t._id,t=t._name):(n=lo(),(e=Jy).time=jn(),t=null==t?null:t+"");for(var r=this._groups,i=r.length,o=0;o<i;++o)for(var u,a=r[o],c=a.length,s=0;s<c;++s)(u=a[s])&&Sy(u,t,n,s,a,e||ho(u,n));return new so(r,this._parents,t,n)};Pi.prototype.interrupt=Ey,
Pi.prototype.transition=Qy;var Ky=[null],tg=function(t,n){var e,r,i=t.__transition;if(i){n=null==n?null:n+"";for(r in i)if((e=i[r]).state>by&&e.name===n)return new so([[t]],Ky,n,+r)}return null},ng=Array.prototype.slice,eg=function(t){return t},rg=1,ig=2,og=3,ug=4,ag=1e-6,cg=function(){function t(t){var o,u=0;t.eachAfter(function(t){var e=t.children;e?(t.x=To(e),t.y=ko(e)):(t.x=o?u+=n(t,o):0,t.y=0,o=t)});var a=Ao(t),c=Eo(t),s=a.x-n(a,c)/2,f=c.x+n(c,a)/2;return t.eachAfter(i?function(n){n.x=(n.x-t.x)*e,n.y=(t.y-n.y)*r}:function(n){n.x=(n.x-s)/(f-s)*e,n.y=(1-(t.y?n.y/t.y:1))*r})}var n=Mo,e=1,r=1,i=!1;return t.separation=function(e){return arguments.length?(n=e,t):n},t.size=function(n){return arguments.length?(i=!1,e=+n[0],r=+n[1],t):i?null:[e,r]},t.nodeSize=function(n){return arguments.length?(i=!0,e=+n[0],r=+n[1],t):i?[e,r]:null},t},sg=function(t){var n,e,r,i,o=this,u=[o];do for(n=u.reverse(),u=[];o=n.pop();)if(t(o),e=o.children)for(r=0,i=e.length;r<i;++r)u.push(e[r]);while(u.length);return this},fg=function(t){for(var n,e,r=this,i=[r];r=i.pop();)if(t(r),n=r.children)for(e=n.length-1;e>=0;--e)i.push(n[e]);return this},lg=function(t){for(var n,e,r,i=this,o=[i],u=[];i=o.pop();)if(u.push(i),n=i.children)for(e=0,r=n.length;e<r;++e)o.push(n[e]);for(;i=u.pop();)t(i);return this},hg=function(t){return this.eachAfter(function(n){for(var e=+t(n.data)||0,r=n.children,i=r&&r.length;--i>=0;)e+=r[i].value;n.value=e})},pg=function(t){return this.eachBefore(function(n){n.children&&n.children.sort(t)})},dg=function(t){for(var n=this,e=Co(n,t),r=[n];n!==e;)n=n.parent,r.push(n);for(var i=r.length;t!==e;)r.splice(i,0,t),t=t.parent;return r},vg=function(){for(var t=this,n=[t];t=t.parent;)n.push(t);return n},_g=function(){var t=[];return this.each(function(n){t.push(n)}),t},yg=function(){var t=[];return this.eachBefore(function(n){n.children||t.push(n)}),t},gg=function(){var t=this,n=[];return t.each(function(e){e!==t&&n.push({source:e.parent,target:e})}),n};Uo.prototype=zo.prototype={constructor:Uo,each:sg,eachAfter:lg,eachBefore:fg,sum:hg,sort:pg,path:dg,ancestors:vg,descendants:_g,leaves:yg,links:gg,copy:Po};var mg=function(t){for(var n,e=(t=t.slice()).length,r=null,i=r;e;){var o=new Do(t[e-1]);i=i?i.next=o:r=o,t[n]=t[--e]}return{head:r,tail:i}},xg=function(t){return Fo(mg(t),[])},bg=function(t){return Wo(t),t},wg=function(t){return function(){return t}},Mg=function(){function t(t){return t.x=e/2,t.y=r/2,n?t.eachBefore(Qo(n)).eachAfter(Ko(i,.5)).eachBefore(tu(1)):t.eachBefore(Qo(Jo)).eachAfter(Ko(Go,1)).eachAfter(Ko(i,t.r/Math.min(e,r))).eachBefore(tu(Math.min(e,r)/(2*t.r))),t}var n=null,e=1,r=1,i=Go;return t.radius=function(e){return arguments.length?(n=$o(e),t):n},t.size=function(n){return arguments.length?(e=+n[0],r=+n[1],t):[e,r]},t.padding=function(n){return arguments.length?(i="function"==typeof n?n:wg(+n),t):i},t},Tg=function(t){t.x0=Math.round(t.x0),t.y0=Math.round(t.y0),t.x1=Math.round(t.x1),t.y1=Math.round(t.y1)},Ng=function(t,n,e,r,i){for(var o,u=t.children,a=-1,c=u.length,s=t.value&&(r-n)/t.value;++a<c;)o=u[a],o.y0=e,o.y1=i,o.x0=n,o.x1=n+=o.value*s},kg=function(){function t(t){var u=t.height+1;return t.x0=t.y0=i,t.x1=e,t.y1=r/u,t.eachBefore(n(r,u)),o&&t.eachBefore(Tg),t}function n(t,n){return function(e){e.children&&Ng(e,e.x0,t*(e.depth+1)/n,e.x1,t*(e.depth+2)/n);var r=e.x0,o=e.y0,u=e.x1-i,a=e.y1-i;u<r&&(r=u=(r+u)/2),a<o&&(o=a=(o+a)/2),e.x0=r,e.y0=o,e.x1=u,e.y1=a}}var e=1,r=1,i=0,o=!1;return t.round=function(n){return arguments.length?(o=!!n,t):o},t.size=function(n){return arguments.length?(e=+n[0],r=+n[1],t):[e,r]},t.padding=function(n){return arguments.length?(i=+n,t):i},t},Sg="$",Ag={depth:-1},Eg={},Cg=function(){function t(t){var r,i,o,u,a,c,s,f=t.length,l=new Array(f),h={};for(i=0;i<f;++i)r=t[i],a=l[i]=new Uo(r),null!=(c=n(r,i,t))&&(c+="")&&(s=Sg+(a.id=c),h[s]=s in h?Eg:a);for(i=0;i<f;++i)if(a=l[i],c=e(t[i],i,t),null!=c&&(c+="")){if(u=h[Sg+c],!u)throw new Error("missing: "+c);if(u===Eg)throw new Error("ambiguous: "+c);u.children?u.children.push(a):u.children=[a],a.parent=u}else{if(o)throw new Error("multiple roots");o=a}if(!o)throw new Error("no root");if(o.parent=Ag,o.eachBefore(function(t){t.depth=t.parent.depth+1,--f}).eachBefore(Ro),o.parent=null,f>0)throw new Error("cycle");return o}var n=nu,e=eu;return t.id=function(e){return arguments.length?(n=Zo(e),t):n},t.parentId=function(n){return arguments.length?(e=Zo(n),t):e},t};su.prototype=Object.create(Uo.prototype);var zg=function(){function t(t){var r=fu(t);if(r.eachAfter(n),r.parent.m=-r.z,r.eachBefore(e),c)t.eachBefore(i);else{var s=t,f=t,l=t;t.eachBefore(function(t){t.x<s.x&&(s=t),t.x>f.x&&(f=t),t.depth>l.depth&&(l=t)});var h=s===f?1:o(s,f)/2,p=h-s.x,d=u/(f.x+h+p),v=a/(l.depth||1);t.eachBefore(function(t){t.x=(t.x+p)*d,t.y=t.depth*v})}return t}function n(t){var n=t.children,e=t.parent.children,i=t.i?e[t.i-1]:null;if(n){au(t);var u=(n[0].z+n[n.length-1].z)/2;i?(t.z=i.z+o(t._,i._),t.m=t.z-u):t.z=u}else i&&(t.z=i.z+o(t._,i._));t.parent.A=r(t,i,t.parent.A||e[0])}function e(t){t._.x=t.z+t.parent.m,t.m+=t.parent.m}function r(t,n,e){if(n){for(var r,i=t,u=t,a=n,c=i.parent.children[0],s=i.m,f=u.m,l=a.m,h=c.m;a=ou(a),i=iu(i),a&&i;)c=iu(c),u=ou(u),u.a=t,r=a.z+l-i.z-s+o(a._,i._),r>0&&(uu(cu(a,t,e),t,r),s+=r,f+=r),l+=a.m,s+=i.m,h+=c.m,f+=u.m;a&&!ou(u)&&(u.t=a,u.m+=l-f),i&&!iu(c)&&(c.t=i,c.m+=s-h,e=t)}return e}function i(t){t.x*=u,t.y=t.depth*a}var o=ru,u=1,a=1,c=null;return t.separation=function(n){return arguments.length?(o=n,t):o},t.size=function(n){return arguments.length?(c=!1,u=+n[0],a=+n[1],t):c?null:[u,a]},t.nodeSize=function(n){return arguments.length?(c=!0,u=+n[0],a=+n[1],t):c?[u,a]:null},t},Pg=function(t,n,e,r,i){for(var o,u=t.children,a=-1,c=u.length,s=t.value&&(i-e)/t.value;++a<c;)o=u[a],o.x0=n,o.x1=r,o.y0=e,o.y1=e+=o.value*s},qg=(1+Math.sqrt(5))/2,Lg=function t(n){function e(t,e,r,i,o){lu(n,t,e,r,i,o)}return e.ratio=function(n){return t((n=+n)>1?n:1)},e}(qg),Rg=function(){function t(t){return t.x0=t.y0=0,t.x1=i,t.y1=o,t.eachBefore(n),u=[0],r&&t.eachBefore(Tg),t}function n(t){var n=u[t.depth],r=t.x0+n,i=t.y0+n,o=t.x1-n,h=t.y1-n;o<r&&(r=o=(r+o)/2),h<i&&(i=h=(i+h)/2),t.x0=r,t.y0=i,t.x1=o,t.y1=h,t.children&&(n=u[t.depth+1]=a(t)/2,r+=l(t)-n,i+=c(t)-n,o-=s(t)-n,h-=f(t)-n,o<r&&(r=o=(r+o)/2),h<i&&(i=h=(i+h)/2),e(t,r,i,o,h))}var e=Lg,r=!1,i=1,o=1,u=[0],a=Go,c=Go,s=Go,f=Go,l=Go;return t.round=function(n){return arguments.length?(r=!!n,t):r},t.size=function(n){return arguments.length?(i=+n[0],o=+n[1],t):[i,o]},t.tile=function(n){return arguments.length?(e=Zo(n),t):e},t.padding=function(n){return arguments.length?t.paddingInner(n).paddingOuter(n):t.paddingInner()},t.paddingInner=function(n){return arguments.length?(a="function"==typeof n?n:wg(+n),t):a},t.paddingOuter=function(n){return arguments.length?t.paddingTop(n).paddingRight(n).paddingBottom(n).paddingLeft(n):t.paddingTop()},t.paddingTop=function(n){return arguments.length?(c="function"==typeof n?n:wg(+n),t):c},t.paddingRight=function(n){return arguments.length?(s="function"==typeof n?n:wg(+n),t):s},t.paddingBottom=function(n){return arguments.length?(f="function"==typeof n?n:wg(+n),t):f},t.paddingLeft=function(n){return arguments.length?(l="function"==typeof n?n:wg(+n),t):l},t},Ug=function(t,n,e,r,i){function o(t,n,e,r,i,u,a){if(t>=n-1){var s=c[t];return s.x0=r,s.y0=i,s.x1=u,s.y1=a,void 0}for(var l=f[t],h=e/2+l,p=t+1,d=n-1;p<d;){var v=p+d>>>1;f[v]<h?p=v+1:d=v}var _=f[p]-l,y=e-_;if(a-i>u-r){var g=(i*y+a*_)/e;o(t,p,_,r,i,u,g),o(p,n,y,r,g,u,a)}else{var m=(r*y+u*_)/e;o(t,p,_,r,i,m,a),o(p,n,y,m,i,u,a)}}var u,a,c=t.children,s=c.length,f=new Array(s+1);for(f[0]=a=u=0;u<s;++u)f[u+1]=a+=c[u].value;o(0,s,t.value,n,e,r,i)},Dg=function(t,n,e,r,i){(1&t.depth?Pg:Ng)(t,n,e,r,i)},Og=function t(n){function e(t,e,r,i,o){if((u=t._squarify)&&u.ratio===n)for(var u,a,c,s,f,l=-1,h=u.length,p=t.value;++l<h;){for(a=u[l],c=a.children,s=a.value=0,f=c.length;s<f;++s)a.value+=c[s].value;a.dice?Ng(a,e,r,i,r+=(o-r)*a.value/p):Pg(a,e,r,e+=(i-e)*a.value/p,o),p-=a.value}else t._squarify=u=lu(n,t,e,r,i,o),u.ratio=n}return e.ratio=function(n){return t((n=+n)>1?n:1)},e}(qg),Fg=function(t,n){function e(){var e,i,o=r.length,u=0,a=0;for(e=0;e<o;++e)i=r[e],u+=i.x,a+=i.y;for(u=u/o-t,a=a/o-n,e=0;e<o;++e)i=r[e],i.x-=u,i.y-=a}var r;return null==t&&(t=0),null==n&&(n=0),e.initialize=function(t){r=t},e.x=function(n){return arguments.length?(t=+n,e):t},e.y=function(t){return arguments.length?(n=+t,e):n},e},Ig=function(t){return function(){return t}},Yg=function(){return 1e-6*(Math.random()-.5)},Bg=function(t){function n(){function t(t,e,r,i,u){var a=t.data,p=t.r,d=l+p;{if(!a)return e>s+d||i<s-d||r>f+d||u<f-d;if(a.index>n){var v=s-a.x-a.vx,_=f-a.y-a.vy,y=v*v+_*_;y<d*d&&(0===v&&(v=Yg(),y+=v*v),0===_&&(_=Yg(),y+=_*_),y=(d-(y=Math.sqrt(y)))/y*o,c.vx+=(v*=y)*(d=(p*=p)/(h+p)),c.vy+=(_*=y)*d,a.vx-=v*(d=1-d),a.vy-=_*d)}}}for(var n,a,c,s,f,l,h,p=r.length,d=0;d<u;++d)for(a=I(r,hu,pu).visitAfter(e),n=0;n<p;++n)c=r[n],l=i[n],h=l*l,s=c.x+c.vx,f=c.y+c.vy,a.visit(t)}function e(t){if(t.data)return t.r=i[t.data.index];for(var n=t.r=0;n<4;++n)t[n]&&t[n].r>t.r&&(t.r=t[n].r)}var r,i,o=1,u=1;return"function"!=typeof t&&(t=Ig(null==t?1:+t)),n.initialize=function(n){var e,o=(r=n).length;for(i=new Array(o),e=0;e<o;++e)i[e]=+t(r[e],e,r)},n.iterations=function(t){return arguments.length?(u=+t,n):u},n.strength=function(t){return arguments.length?(o=+t,n):o},n.radius=function(e){return arguments.length?(t="function"==typeof e?e:Ig(+e),n):t},n},jg=function(t){function n(t){return 1/Math.min(f[t.source.index],f[t.target.index])}function e(n){for(var e=0,r=t.length;e<v;++e)for(var i,o,u,s,f,h,p,d=0;d<r;++d)i=t[d],o=i.source,u=i.target,s=u.x+u.vx-o.x-o.vx||Yg(),f=u.y+u.vy-o.y-o.vy||Yg(),h=Math.sqrt(s*s+f*f),h=(h-c[d])/h*n*a[d],s*=h,f*=h,u.vx-=s*(p=l[d]),u.vy-=f*p,o.vx+=s*(p=1-p),o.vy+=f*p}function r(){if(s){var n,e,r=s.length,p=t.length,d=o(s,h);for(n=0,f=new Array(r);n<r;++n)f[n]=0;for(n=0;n<p;++n)e=t[n],e.index=n,"object"!=typeof e.source&&(e.source=d.get(e.source)),"object"!=typeof e.target&&(e.target=d.get(e.target)),++f[e.source.index],++f[e.target.index];for(n=0,l=new Array(p);n<p;++n)e=t[n],l[n]=f[e.source.index]/(f[e.source.index]+f[e.target.index]);a=new Array(p),i(),c=new Array(p),u()}}function i(){if(s)for(var n=0,e=t.length;n<e;++n)a[n]=+p(t[n],n,t)}function u(){if(s)for(var n=0,e=t.length;n<e;++n)c[n]=+d(t[n],n,t)}var a,c,s,f,l,h=du,p=n,d=Ig(30),v=1;return null==t&&(t=[]),e.initialize=function(t){s=t,r()},e.links=function(n){return arguments.length?(t=n,r(),e):t},e.id=function(t){return arguments.length?(h=t,e):h},e.iterations=function(t){return arguments.length?(v=+t,e):v},e.strength=function(t){return arguments.length?(p="function"==typeof t?t:Ig(+t),i(),e):p},e.distance=function(t){return arguments.length?(d="function"==typeof t?t:Ig(+t),u(),e):d},e},Hg=10,Xg=Math.PI*(3-Math.sqrt(5)),Vg=function(t){function n(){e(),d.call("tick",u),a<c&&(p.stop(),d.call("end",u))}function e(){var n,e,r=t.length;for(a+=(f-a)*s,h.each(function(t){t(a)}),n=0;n<r;++n)e=t[n],null==e.fx?e.x+=e.vx*=l:(e.x=e.fx,e.vx=0),null==e.fy?e.y+=e.vy*=l:(e.y=e.fy,e.vy=0)}function r(){for(var n,e=0,r=t.length;e<r;++e){if(n=t[e],n.index=e,isNaN(n.x)||isNaN(n.y)){var i=Hg*Math.sqrt(e),o=e*Xg;n.x=i*Math.cos(o),n.y=i*Math.sin(o)}(isNaN(n.vx)||isNaN(n.vy))&&(n.vx=n.vy=0)}}function i(n){return n.initialize&&n.initialize(t),n}var u,a=1,c=.001,s=1-Math.pow(c,1/300),f=0,l=.6,h=o(),p=Vn(n),d=Pn("tick","end");return null==t&&(t=[]),r(),u={tick:e,restart:function(){return p.restart(n),u},stop:function(){return p.stop(),u},nodes:function(n){return arguments.length?(t=n,r(),h.each(i),u):t},alpha:function(t){return arguments.length?(a=+t,u):a},alphaMin:function(t){return arguments.length?(c=+t,u):c},alphaDecay:function(t){return arguments.length?(s=+t,u):+s},alphaTarget:function(t){return arguments.length?(f=+t,u):f},velocityDecay:function(t){return arguments.length?(l=1-t,u):1-l},force:function(t,n){return arguments.length>1?(null==n?h.remove(t):h.set(t,i(n)),u):h.get(t)},find:function(n,e,r){var i,o,u,a,c,s=0,f=t.length;for(null==r?r=1/0:r*=r,s=0;s<f;++s)a=t[s],i=n-a.x,o=e-a.y,u=i*i+o*o,u<r&&(c=a,r=u);return c},on:function(t,n){return arguments.length>1?(d.on(t,n),u):d.on(t)}}},Wg=function(){function t(t){var n,a=i.length,c=I(i,vu,_u).visitAfter(e);for(u=t,n=0;n<a;++n)o=i[n],c.visit(r)}function n(){if(i){var t,n=i.length;for(a=new Array(n),t=0;t<n;++t)a[t]=+c(i[t],t,i)}}function e(t){var n,e,r,i,o,u=0;if(t.length){for(r=i=o=0;o<4;++o)(n=t[o])&&(e=n.value)&&(u+=e,r+=e*n.x,i+=e*n.y);t.x=r/u,t.y=i/u}else{n=t,n.x=n.data.x,n.y=n.data.y;do u+=a[n.data.index];while(n=n.next)}t.value=u}function r(t,n,e,r){if(!t.value)return!0;var i=t.x-o.x,c=t.y-o.y,h=r-n,p=i*i+c*c;if(h*h/l<p)return p<f&&(0===i&&(i=Yg(),p+=i*i),0===c&&(c=Yg(),p+=c*c),p<s&&(p=Math.sqrt(s*p)),o.vx+=i*t.value*u/p,o.vy+=c*t.value*u/p),!0;if(!(t.length||p>=f)){(t.data!==o||t.next)&&(0===i&&(i=Yg(),p+=i*i),0===c&&(c=Yg(),p+=c*c),p<s&&(p=Math.sqrt(s*p)));do t.data!==o&&(h=a[t.data.index]*u/p,o.vx+=i*h,o.vy+=c*h);while(t=t.next)}}var i,o,u,a,c=Ig(-30),s=1,f=1/0,l=.81;return t.initialize=function(t){i=t,n()},t.strength=function(e){return arguments.length?(c="function"==typeof e?e:Ig(+e),n(),t):c},t.distanceMin=function(n){return arguments.length?(s=n*n,t):Math.sqrt(s)},t.distanceMax=function(n){return arguments.length?(f=n*n,t):Math.sqrt(f)},t.theta=function(n){return arguments.length?(l=n*n,t):Math.sqrt(l)},t},$g=function(t){function n(t){for(var n,e=0,u=r.length;e<u;++e)n=r[e],n.vx+=(o[e]-n.x)*i[e]*t}function e(){if(r){var n,e=r.length;for(i=new Array(e),o=new Array(e),n=0;n<e;++n)i[n]=isNaN(o[n]=+t(r[n],n,r))?0:+u(r[n],n,r)}}var r,i,o,u=Ig(.1);return"function"!=typeof t&&(t=Ig(null==t?0:+t)),n.initialize=function(t){r=t,e()},n.strength=function(t){return arguments.length?(u="function"==typeof t?t:Ig(+t),e(),n):u},n.x=function(r){return arguments.length?(t="function"==typeof r?r:Ig(+r),e(),n):t},n},Zg=function(t){function n(t){for(var n,e=0,u=r.length;e<u;++e)n=r[e],n.vy+=(o[e]-n.y)*i[e]*t}function e(){if(r){var n,e=r.length;for(i=new Array(e),o=new Array(e),n=0;n<e;++n)i[n]=isNaN(o[n]=+t(r[n],n,r))?0:+u(r[n],n,r)}}var r,i,o,u=Ig(.1);return"function"!=typeof t&&(t=Ig(null==t?0:+t)),n.initialize=function(t){r=t,e()},n.strength=function(t){return arguments.length?(u="function"==typeof t?t:Ig(+t),e(),n):u},n.y=function(r){return arguments.length?(t="function"==typeof r?r:Ig(+r),e(),n):t},n},Gg=function(){t.event.preventDefault(),t.event.stopImmediatePropagation()},Jg=function(t){var n=t.document.documentElement,e=dy(t).on("dragstart.drag",Gg,!0);"onselectstart"in n?e.on("selectstart.drag",Gg,!0):(n.__noselect=n.style.MozUserSelect,n.style.MozUserSelect="none")},Qg=function(t){return function(){return t}};mu.prototype.on=function(){var t=this._.on.apply(this._,arguments);return t===this._?this:t};var Kg=function(){function n(t){t.on("mousedown.drag",e).on("touchstart.drag",o).on("touchmove.drag",u).on("touchend.drag touchcancel.drag",a).style("-webkit-tap-highlight-color","rgba(0,0,0,0)")}function e(){if(!f&&l.apply(this,arguments)){var n=c("mouse",h.apply(this,arguments),P_,this,arguments);n&&(dy(t.event.view).on("mousemove.drag",r,!0).on("mouseup.drag",i,!0),Jg(t.event.view),yu(),s=!1,n("start"))}}function r(){Gg(),s=!0,d.mouse("drag")}function i(){dy(t.event.view).on("mousemove.drag mouseup.drag",null),gu(t.event.view,s),Gg(),d.mouse("end")}function o(){if(l.apply(this,arguments)){var n,e,r=t.event.changedTouches,i=h.apply(this,arguments),o=r.length;for(n=0;n<o;++n)(e=c(r[n].identifier,i,_y,this,arguments))&&(yu(),e("start"))}}function u(){var n,e,r=t.event.changedTouches,i=r.length;for(n=0;n<i;++n)(e=d[r[n].identifier])&&(Gg(),e("drag"))}function a(){var n,e,r=t.event.changedTouches,i=r.length;for(f&&clearTimeout(f),f=setTimeout(function(){f=null},500),n=0;n<i;++n)(e=d[r[n].identifier])&&(yu(),e("end"))}function c(e,r,i,o,u){var a,c,s,f=i(r,e),l=v.copy();if(Xr(new mu(n,"beforestart",a,e,_,f[0],f[1],0,0,l),function(){return null!=(t.event.subject=a=p.apply(o,u))&&(c=a.x-f[0]||0,s=a.y-f[1]||0,!0)}))return function t(h){var p,v=f;switch(h){case"start":d[e]=t,p=_++;break;case"end":delete d[e],--_;case"drag":f=i(r,e),p=_}Xr(new mu(n,h,a,e,p,f[0]+c,f[1]+s,f[0]-v[0],f[1]-v[1],l),l.apply,l,[h,o,u])}}var s,f,l=xu,h=bu,p=wu,d={},v=Pn("start","drag","end"),_=0;return n.filter=function(t){return arguments.length?(l="function"==typeof t?t:Qg(!!t),n):l},n.container=function(t){return arguments.length?(h="function"==typeof t?t:Qg(t),n):h},n.subject=function(t){return arguments.length?(p="function"==typeof t?t:Qg(t),n):p},n.on=function(){var t=v.on.apply(v,arguments);return t===v?n:t},n},tm=function(t){return function(){return t}};Nu.prototype={constructor:Nu,insert:function(t,n){var e,r,i;if(t){if(n.P=t,n.N=t.N,t.N&&(t.N.P=n),t.N=n,t.R){for(t=t.R;t.L;)t=t.L;t.L=n}else t.R=n;e=t}else this._?(t=Eu(this._),n.P=null,n.N=t,t.P=t.L=n,e=t):(n.P=n.N=null,this._=n,e=null);for(n.L=n.R=null,n.U=e,n.C=!0,t=n;e&&e.C;)r=e.U,e===r.L?(i=r.R,i&&i.C?(e.C=i.C=!1,r.C=!0,t=r):(t===e.R&&(Su(this,e),t=e,e=t.U),e.C=!1,r.C=!0,Au(this,r))):(i=r.L,i&&i.C?(e.C=i.C=!1,r.C=!0,t=r):(t===e.L&&(Au(this,e),t=e,e=t.U),e.C=!1,r.C=!0,Su(this,r))),e=t.U;this._.C=!1},remove:function(t){t.N&&(t.N.P=t.P),t.P&&(t.P.N=t.N),t.N=t.P=null;var n,e,r,i=t.U,o=t.L,u=t.R;if(e=o?u?Eu(u):o:u,i?i.L===t?i.L=e:i.R=e:this._=e,o&&u?(r=e.C,e.C=t.C,e.L=o,o.U=e,e!==u?(i=e.U,e.U=t.U,t=e.R,i.L=t,e.R=u,u.U=e):(e.U=i,i=e,t=e.R)):(r=t.C,t=e),t&&(t.U=i),!r){if(t&&t.C)return void(t.C=!1);do{if(t===this._)break;if(t===i.L){if(n=i.R,n.C&&(n.C=!1,i.C=!0,Su(this,i),n=i.R),n.L&&n.L.C||n.R&&n.R.C){n.R&&n.R.C||(n.L.C=!1,n.C=!0,Au(this,n),n=i.R),n.C=i.C,i.C=n.R.C=!1,Su(this,i),t=this._;break}}else if(n=i.L,n.C&&(n.C=!1,i.C=!0,Au(this,i),n=i.L),n.L&&n.L.C||n.R&&n.R.C){n.L&&n.L.C||(n.R.C=!1,n.C=!0,Su(this,n),n=i.L),n.C=i.C,i.C=n.L.C=!1,Au(this,i),t=this._;break}n.C=!0,t=i,i=i.U}while(!t.C);t&&(t.C=!1)}}};var nm,em,rm,im,om,um=[],am=[],cm=1e-6,sm=1e-12;ta.prototype={constructor:ta,polygons:function(){var t=this.edges;return this.cells.map(function(n){var e=n.halfedges.map(function(e){return Ou(n,t[e])});return e.data=n.site.data,e})},triangles:function(){var t=[],n=this.edges;return this.cells.forEach(function(e,r){for(var i,o=e.site,u=e.halfedges,a=-1,c=u.length,s=n[u[c-1]],f=s.left===o?s.right:s.left;++a<c;)i=f,s=n[u[a]],f=s.left===o?s.right:s.left,r<i.index&&r<f.index&&Qu(o,i,f)<0&&t.push([o.data,i.data,f.data])}),t},links:function(){return this.edges.filter(function(t){return t.right}).map(function(t){return{source:t.left.data,target:t.right.data}})}};var fm=function(){function t(t){return new ta(t.map(function(r,i){var o=[Math.round(n(r,i,t)/cm)*cm,Math.round(e(r,i,t)/cm)*cm];return o.index=i,o.data=r,o}),r)}var n=Mu,e=Tu,r=null;return t.polygons=function(n){return t(n).polygons()},t.links=function(n){return t(n).links()},t.triangles=function(n){return t(n).triangles()},t.x=function(e){return arguments.length?(n="function"==typeof e?e:tm(+e),t):n},t.y=function(n){return arguments.length?(e="function"==typeof n?n:tm(+n),t):e},t.extent=function(n){return arguments.length?(r=null==n?null:[[+n[0][0],+n[0][1]],[+n[1][0],+n[1][1]]],t):r&&[[r[0][0],r[0][1]],[r[1][0],r[1][1]]]},t.size=function(n){return arguments.length?(r=null==n?null:[[0,0],[+n[0],+n[1]]],t):r&&[r[1][0]-r[0][0],r[1][1]-r[0][1]]},t},lm=function(t){return function(){return t}};ea.prototype={constructor:ea,scale:function(t){return 1===t?this:new ea(this.k*t,this.x,this.y)},translate:function(t,n){return 0===t&0===n?this:new ea(this.k,this.x+this.k*t,this.y+this.k*n)},apply:function(t){return[t[0]*this.k+this.x,t[1]*this.k+this.y]},applyX:function(t){return t*this.k+this.x},applyY:function(t){return t*this.k+this.y},invert:function(t){return[(t[0]-this.x)/this.k,(t[1]-this.y)/this.k]},invertX:function(t){return(t-this.x)/this.k},invertY:function(t){return(t-this.y)/this.k},rescaleX:function(t){return t.copy().domain(t.range().map(this.invertX,this).map(t.invert,t))},rescaleY:function(t){return t.copy().domain(t.range().map(this.invertY,this).map(t.invert,t))},toString:function(){return"translate("+this.x+","+this.y+") scale("+this.k+")"}};var hm=new ea(1,0,0);ra.prototype=ea.prototype;var pm=function(){t.event.preventDefault(),t.event.stopImmediatePropagation()},dm=function(){function n(t){t.on("wheel.zoom",s).on("mousedown.zoom",f).on("dblclick.zoom",l).on("touchstart.zoom",h).on("touchmove.zoom",p).on("touchend.zoom touchcancel.zoom",d).style("-webkit-tap-highlight-color","rgba(0,0,0,0)").property("__zoom",aa)}function e(t,n){return n=Math.max(m,Math.min(x,n)),n===t.k?t:new ea(n,t.x,t.y)}function r(t,n,e){var r=n[0]-e[0]*t.k,i=n[1]-e[1]*t.k;return r===t.x&&i===t.y?t:new ea(t.k,r,i)}function i(t,n){var e=Math.min(0,t.invertX(n[0][0])-b)||Math.max(0,t.invertX(n[1][0])-w),r=Math.min(0,t.invertY(n[0][1])-M)||Math.max(0,t.invertY(n[1][1])-T);return e||r?t.translate(e,r):t}function o(t){return[(+t[0][0]+ +t[1][0])/2,(+t[0][1]+ +t[1][1])/2]}function u(t,n,e){t.on("start.zoom",function(){a(this,arguments).start()}).on("interrupt.zoom end.zoom",function(){a(this,arguments).end()}).tween("zoom",function(){var t=this,r=arguments,i=a(t,r),u=g.apply(t,r),c=e||o(u),s=Math.max(u[1][0]-u[0][0],u[1][1]-u[0][1]),f=t.__zoom,l="function"==typeof n?n.apply(t,r):n,h=Mp(f.invert(c).concat(s/f.k),l.invert(c).concat(s/l.k));return function(t){if(1===t)t=l;else{var n=h(t),e=s/n[2];t=new ea(e,c[0]-n[0]*e,c[1]-n[1]*e)}i.zoom(null,t)}})}function a(t,n){for(var e,r=0,i=k.length;r<i;++r)if((e=k[r]).that===t)return e;return new c(t,n)}function c(t,n){this.that=t,this.args=n,this.index=-1,this.active=0,this.extent=g.apply(t,n)}function s(){function n(){o.wheel=null,o.end()}if(y.apply(this,arguments)){var o=a(this,arguments),u=this.__zoom,c=Math.max(m,Math.min(x,u.k*Math.pow(2,-t.event.deltaY*(t.event.deltaMode?120:1)/500))),s=P_(this);if(o.wheel)o.mouse[0][0]===s[0]&&o.mouse[0][1]===s[1]||(o.mouse[1]=u.invert(o.mouse[0]=s)),clearTimeout(o.wheel);else{if(u.k===c)return;o.mouse=[s,u.invert(s)],Ay(this),o.start()}pm(),o.wheel=setTimeout(n,E),o.zoom("mouse",i(r(e(u,c),o.mouse[0],o.mouse[1]),o.extent))}}function f(){function n(){pm(),o.moved=!0,o.zoom("mouse",i(r(o.that.__zoom,o.mouse[0]=P_(o.that),o.mouse[1]),o.extent))}function e(){u.on("mousemove.zoom mouseup.zoom",null),gu(t.event.view,o.moved),pm(),o.end()}if(!_&&y.apply(this,arguments)){var o=a(this,arguments),u=dy(t.event.view).on("mousemove.zoom",n,!0).on("mouseup.zoom",e,!0),c=P_(this);Jg(t.event.view),ia(),o.mouse=[c,this.__zoom.invert(c)],Ay(this),o.start()}}function l(){if(y.apply(this,arguments)){var o=this.__zoom,a=P_(this),c=o.invert(a),s=o.k*(t.event.shiftKey?.5:2),f=i(r(e(o,s),a,c),g.apply(this,arguments));pm(),N>0?dy(this).transition().duration(N).call(u,f,a):dy(this).call(n.transform,f)}}function h(){if(y.apply(this,arguments)){var n,e,r,i=a(this,arguments),o=t.event.changedTouches,u=o.length;for(ia(),n=0;n<u;++n)e=o[n],r=_y(this,o,e.identifier),r=[r,this.__zoom.invert(r),e.identifier],i.touch0?i.touch1||(i.touch1=r):i.touch0=r;return v&&(v=clearTimeout(v),!i.touch1)?(i.end(),l.apply(this,arguments)):void(t.event.touches.length===u&&(v=setTimeout(function(){v=null},A),Ay(this),i.start()))}}function p(){var n,o,u,c,s=a(this,arguments),f=t.event.changedTouches,l=f.length;for(pm(),v&&(v=clearTimeout(v)),n=0;n<l;++n)o=f[n],u=_y(this,f,o.identifier),s.touch0&&s.touch0[2]===o.identifier?s.touch0[0]=u:s.touch1&&s.touch1[2]===o.identifier&&(s.touch1[0]=u);if(o=s.that.__zoom,s.touch1){var h=s.touch0[0],p=s.touch0[1],d=s.touch1[0],_=s.touch1[1],y=(y=d[0]-h[0])*y+(y=d[1]-h[1])*y,g=(g=_[0]-p[0])*g+(g=_[1]-p[1])*g;o=e(o,Math.sqrt(y/g)),u=[(h[0]+d[0])/2,(h[1]+d[1])/2],c=[(p[0]+_[0])/2,(p[1]+_[1])/2]}else{if(!s.touch0)return;u=s.touch0[0],c=s.touch0[1]}s.zoom("touch",i(r(o,u,c),s.extent))}function d(){var n,e,r=a(this,arguments),i=t.event.changedTouches,o=i.length;for(ia(),_&&clearTimeout(_),_=setTimeout(function(){_=null},A),n=0;n<o;++n)e=i[n],r.touch0&&r.touch0[2]===e.identifier?delete r.touch0:r.touch1&&r.touch1[2]===e.identifier&&delete r.touch1;r.touch1&&!r.touch0&&(r.touch0=r.touch1,delete r.touch1),r.touch0||r.end()}var v,_,y=oa,g=ua,m=0,x=1/0,b=-x,w=x,M=b,T=w,N=250,k=[],S=Pn("start","zoom","end"),A=500,E=150;return n.transform=function(t,n){var e=t.selection?t.selection():t;e.property("__zoom",aa),t!==e?u(t,n):e.interrupt().each(function(){a(this,arguments).start().zoom(null,"function"==typeof n?n.apply(this,arguments):n).end()})},n.scaleBy=function(t,e){n.scaleTo(t,function(){var t=this.__zoom.k,n="function"==typeof e?e.apply(this,arguments):e;return t*n})},n.scaleTo=function(t,u){n.transform(t,function(){var t=g.apply(this,arguments),n=this.__zoom,a=o(t),c=n.invert(a),s="function"==typeof u?u.apply(this,arguments):u;return i(r(e(n,s),a,c),t)})},n.translateBy=function(t,e,r){n.transform(t,function(){return i(this.__zoom.translate("function"==typeof e?e.apply(this,arguments):e,"function"==typeof r?r.apply(this,arguments):r),g.apply(this,arguments))})},c.prototype={start:function(){return 1===++this.active&&(this.index=k.push(this)-1,this.emit("start")),this},zoom:function(t,n){return this.mouse&&"mouse"!==t&&(this.mouse[1]=n.invert(this.mouse[0])),this.touch0&&"touch"!==t&&(this.touch0[1]=n.invert(this.touch0[0])),this.touch1&&"touch"!==t&&(this.touch1[1]=n.invert(this.touch1[0])),this.that.__zoom=n,this.emit("zoom"),this},end:function(){return 0===--this.active&&(k.splice(this.index,1),this.index=-1,this.emit("end")),this},emit:function(t){Xr(new na(n,t,this.that.__zoom),S.apply,S,[t,this.that,this.args])}},n.filter=function(t){return arguments.length?(y="function"==typeof t?t:lm(!!t),n):y},n.extent=function(t){return arguments.length?(g="function"==typeof t?t:lm([[+t[0][0],+t[0][1]],[+t[1][0],+t[1][1]]]),n):g},n.scaleExtent=function(t){return arguments.length?(m=+t[0],x=+t[1],n):[m,x]},n.translateExtent=function(t){return arguments.length?(b=+t[0][0],w=+t[1][0],M=+t[0][1],T=+t[1][1],n):[[b,M],[w,T]]},n.duration=function(t){return arguments.length?(N=+t,n):N},n.on=function(){var t=S.on.apply(S,arguments);return t===S?n:t},n},vm=function(t){return function(){return t}},_m=function(t,n,e){this.target=t,this.type=n,this.selection=e},ym=function(){t.event.preventDefault(),t.event.stopImmediatePropagation()},gm={name:"drag"},mm={name:"space"},xm={name:"handle"},bm={name:"center"},wm={name:"x",handles:["e","w"].map(sa),input:function(t,n){return t&&[[t[0],n[0][1]],[t[1],n[1][1]]]},output:function(t){return t&&[t[0][0],t[1][0]]}},Mm={name:"y",handles:["n","s"].map(sa),input:function(t,n){return t&&[[n[0][0],t[0]],[n[1][0],t[1]]]},output:function(t){return t&&[t[0][1],t[1][1]]}},Tm={name:"xy",handles:["n","e","s","w","nw","ne","se","sw"].map(sa),input:function(t){return t},output:function(t){return t}},Nm={overlay:"crosshair",selection:"move",n:"ns-resize",e:"ew-resize",s:"ns-resize",w:"ew-resize",nw:"nwse-resize",ne:"nesw-resize",se:"nwse-resize",sw:"nesw-resize"},km={e:"w",w:"e",nw:"ne",ne:"nw",se:"sw",sw:"se"},Sm={n:"s",s:"n",nw:"sw",ne:"se",se:"ne",sw:"nw"},Am={overlay:1,selection:1,n:null,e:1,s:null,w:-1,nw:-1,ne:1,se:1,sw:-1},Em={overlay:1,selection:1,n:-1,e:null,s:1,w:null,nw:-1,ne:-1,se:1,sw:1},Cm=function(){return ya(Tm)},zm=Math.cos,Pm=Math.sin,qm=Math.PI,Lm=qm/2,Rm=2*qm,Um=Math.max,Dm=function(){function t(t){var o,u,a,c,s,f,l=t.length,h=[],p=Ls(l),d=[],v=[],_=v.groups=new Array(l),y=new Array(l*l);for(o=0,s=-1;++s<l;){for(u=0,f=-1;++f<l;)u+=t[s][f];h.push(u),d.push(Ls(l)),o+=u}for(e&&p.sort(function(t,n){return e(h[t],h[n])}),r&&d.forEach(function(n,e){n.sort(function(n,i){return r(t[e][n],t[e][i])})}),o=Um(0,Rm-n*l)/o,c=o?n:Rm/l,u=0,s=-1;++s<l;){for(a=u,f=-1;++f<l;){var g=p[s],m=d[g][f],x=t[g][m],b=u,w=u+=x*o;y[m*l+g]={index:g,subindex:m,startAngle:b,endAngle:w,value:x}}_[g]={index:g,startAngle:a,endAngle:u,value:h[g]},u+=c}for(s=-1;++s<l;)for(f=s-1;++f<l;){var M=y[f*l+s],T=y[s*l+f];(M.value||T.value)&&v.push(M.value<T.value?{source:T,target:M}:{source:M,target:T})}return i?v.sort(i):v}var n=0,e=null,r=null,i=null;return t.padAngle=function(e){return arguments.length?(n=Um(0,e),t):n},t.sortGroups=function(n){return arguments.length?(e=n,t):e},t.sortSubgroups=function(n){return arguments.length?(r=n,t):r},t.sortChords=function(n){return arguments.length?(null==n?i=null:(i=ga(n))._=n,t):i&&i._},t},Om=Array.prototype.slice,Fm=function(t){return function(){return t}},Im=function(){function t(){var t,a=Om.call(arguments),c=n.apply(this,a),s=e.apply(this,a),f=+r.apply(this,(a[0]=c,a)),l=i.apply(this,a)-Lm,h=o.apply(this,a)-Lm,p=f*zm(l),d=f*Pm(l),v=+r.apply(this,(a[0]=s,a)),_=i.apply(this,a)-Lm,y=o.apply(this,a)-Lm;if(u||(u=t=L()),u.moveTo(p,d),u.arc(0,0,f,l,h),l===_&&h===y||(u.quadraticCurveTo(0,0,v*zm(_),v*Pm(_)),u.arc(0,0,v,_,y)),u.quadraticCurveTo(0,0,p,d),u.closePath(),t)return u=null,t+""||null}var n=ma,e=xa,r=ba,i=wa,o=Ma,u=null;return t.radius=function(n){return arguments.length?(r="function"==typeof n?n:Fm(+n),t):r},t.startAngle=function(n){return arguments.length?(i="function"==typeof n?n:Fm(+n),t):i},t.endAngle=function(n){return arguments.length?(o="function"==typeof n?n:Fm(+n),t):o},t.source=function(e){return arguments.length?(n=e,t):n},t.target=function(n){return arguments.length?(e=n,t):e},t.context=function(n){return arguments.length?(u=null==n?null:n,t):u},t},Ym=function(){return new Ta};Ta.prototype={constructor:Ta,reset:function(){this.s=this.t=0},add:function(t){Na(mx,t,this.t),Na(this,mx.s,this.s),this.s?this.t+=mx.t:this.s=mx.t},valueOf:function(){return this.s}};var Bm,jm,Hm,Xm,Vm,Wm,$m,Zm,Gm,Jm,Qm,Km,tx,nx,ex,rx,ix,ox,ux,ax,cx,sx,fx,lx,hx,px,dx,vx,_x,yx,gx,mx=new Ta,xx=1e-6,bx=1e-12,wx=Math.PI,Mx=wx/2,Tx=wx/4,Nx=2*wx,kx=180/wx,Sx=wx/180,Ax=Math.abs,Ex=Math.atan,Cx=Math.atan2,zx=Math.cos,Px=Math.ceil,qx=Math.exp,Lx=Math.log,Rx=Math.pow,Ux=Math.sin,Dx=Math.sign||function(t){return t>0?1:t<0?-1:0},Ox=Math.sqrt,Fx=Math.tan,Ix={Feature:function(t,n){Ca(t.geometry,n)},FeatureCollection:function(t,n){for(var e=t.features,r=-1,i=e.length;++r<i;)Ca(e[r].geometry,n)}},Yx={Sphere:function(t,n){n.sphere()},Point:function(t,n){t=t.coordinates,n.point(t[0],t[1],t[2])},MultiPoint:function(t,n){for(var e=t.coordinates,r=-1,i=e.length;++r<i;)t=e[r],n.point(t[0],t[1],t[2])},LineString:function(t,n){za(t.coordinates,n,0)},MultiLineString:function(t,n){for(var e=t.coordinates,r=-1,i=e.length;++r<i;)za(e[r],n,0)},Polygon:function(t,n){Pa(t.coordinates,n)},MultiPolygon:function(t,n){for(var e=t.coordinates,r=-1,i=e.length;++r<i;)Pa(e[r],n)},GeometryCollection:function(t,n){for(var e=t.geometries,r=-1,i=e.length;++r<i;)Ca(e[r],n)}},Bx=function(t,n){t&&Ix.hasOwnProperty(t.type)?Ix[t.type](t,n):Ca(t,n)},jx=Ym(),Hx=Ym(),Xx={point:Ea,lineStart:Ea,lineEnd:Ea,polygonStart:function(){jx.reset(),Xx.lineStart=qa,Xx.lineEnd=La},polygonEnd:function(){var t=+jx;Hx.add(t<0?Nx+t:t),this.lineStart=this.lineEnd=this.point=Ea},sphere:function(){Hx.add(Nx)}},Vx=function(t){return Hx.reset(),Bx(t,Xx),2*Hx},Wx=Ym(),$x={point:Ha,lineStart:Va,lineEnd:Wa,polygonStart:function(){$x.point=$a,$x.lineStart=Za,$x.lineEnd=Ga,Wx.reset(),Xx.polygonStart()},polygonEnd:function(){Xx.polygonEnd(),$x.point=Ha,$x.lineStart=Va,$x.lineEnd=Wa,jx<0?(Wm=-(Zm=180),$m=-(Gm=90)):Wx>xx?Gm=90:Wx<-xx&&($m=-90),ex[0]=Wm,ex[1]=Zm}},Zx=function(t){var n,e,r,i,o,u,a;if(Gm=Zm=-(Wm=$m=1/0),nx=[],Bx(t,$x),e=nx.length){for(nx.sort(Qa),n=1,r=nx[0],o=[r];n<e;++n)i=nx[n],Ka(r,i[0])||Ka(r,i[1])?(Ja(r[0],i[1])>Ja(r[0],r[1])&&(r[1]=i[1]),Ja(i[0],r[1])>Ja(r[0],r[1])&&(r[0]=i[0])):o.push(r=i);for(u=-(1/0),e=o.length-1,n=0,r=o[e];n<=e;r=i,++n)i=o[n],(a=Ja(r[1],i[0]))>u&&(u=a,Wm=i[0],Zm=r[1])}return nx=ex=null,Wm===1/0||$m===1/0?[[NaN,NaN],[NaN,NaN]]:[[Wm,$m],[Zm,Gm]]},Gx={sphere:Ea,point:tc,lineStart:ec,lineEnd:oc,polygonStart:function(){Gx.lineStart=uc,Gx.lineEnd=ac},polygonEnd:function(){Gx.lineStart=ec,Gx.lineEnd=oc}},Jx=function(t){rx=ix=ox=ux=ax=cx=sx=fx=lx=hx=px=0,Bx(t,Gx);var n=lx,e=hx,r=px,i=n*n+e*e+r*r;return i<bx&&(n=cx,e=sx,r=fx,ix<xx&&(n=ox,e=ux,r=ax),i=n*n+e*e+r*r,i<bx)?[NaN,NaN]:[Cx(e,n)*kx,Sa(r/Ox(i))*kx];
},Qx=function(t){return function(){return t}},Kx=function(t,n){function e(e,r){return e=t(e,r),n(e[0],e[1])}return t.invert&&n.invert&&(e.invert=function(e,r){return e=n.invert(e,r),e&&t.invert(e[0],e[1])}),e};fc.invert=fc;var tb,nb,eb,rb,ib,ob,ub,ab,cb,sb,fb,lb=function(t){function n(n){return n=t(n[0]*Sx,n[1]*Sx),n[0]*=kx,n[1]*=kx,n}return t=lc(t[0]*Sx,t[1]*Sx,t.length>2?t[2]*Sx:0),n.invert=function(n){return n=t.invert(n[0]*Sx,n[1]*Sx),n[0]*=kx,n[1]*=kx,n},n},hb=function(){function t(t,n){e.push(t=r(t,n)),t[0]*=kx,t[1]*=kx}function n(){var t=i.apply(this,arguments),n=o.apply(this,arguments)*Sx,c=u.apply(this,arguments)*Sx;return e=[],r=lc(-t[0]*Sx,-t[1]*Sx,0).invert,vc(a,n,c,1),t={type:"Polygon",coordinates:[e]},e=r=null,t}var e,r,i=Qx([0,0]),o=Qx(90),u=Qx(6),a={point:t};return n.center=function(t){return arguments.length?(i="function"==typeof t?t:Qx([+t[0],+t[1]]),n):i},n.radius=function(t){return arguments.length?(o="function"==typeof t?t:Qx(+t),n):o},n.precision=function(t){return arguments.length?(u="function"==typeof t?t:Qx(+t),n):u},n},pb=function(){var t,n=[];return{point:function(n,e){t.push([n,e])},lineStart:function(){n.push(t=[])},lineEnd:Ea,rejoin:function(){n.length>1&&n.push(n.pop().concat(n.shift()))},result:function(){var e=n;return n=[],t=null,e}}},db=function(t,n,e,r,i,o){var u,a=t[0],c=t[1],s=n[0],f=n[1],l=0,h=1,p=s-a,d=f-c;if(u=e-a,p||!(u>0)){if(u/=p,p<0){if(u<l)return;u<h&&(h=u)}else if(p>0){if(u>h)return;u>l&&(l=u)}if(u=i-a,p||!(u<0)){if(u/=p,p<0){if(u>h)return;u>l&&(l=u)}else if(p>0){if(u<l)return;u<h&&(h=u)}if(u=r-c,d||!(u>0)){if(u/=d,d<0){if(u<l)return;u<h&&(h=u)}else if(d>0){if(u>h)return;u>l&&(l=u)}if(u=o-c,d||!(u<0)){if(u/=d,d<0){if(u>h)return;u>l&&(l=u)}else if(d>0){if(u<l)return;u<h&&(h=u)}return l>0&&(t[0]=a+l*p,t[1]=c+l*d),h<1&&(n[0]=a+h*p,n[1]=c+h*d),!0}}}}},vb=function(t,n){return Ax(t[0]-n[0])<xx&&Ax(t[1]-n[1])<xx},_b=function(t,n,e,r,i){var o,u,a=[],c=[];if(t.forEach(function(t){if(!((n=t.length-1)<=0)){var n,e,r=t[0],u=t[n];if(vb(r,u)){for(i.lineStart(),o=0;o<n;++o)i.point((r=t[o])[0],r[1]);return void i.lineEnd()}a.push(e=new yc(r,t,null,!0)),c.push(e.o=new yc(r,null,e,!1)),a.push(e=new yc(u,t,null,!1)),c.push(e.o=new yc(u,null,e,!0))}}),a.length){for(c.sort(n),gc(a),gc(c),o=0,u=c.length;o<u;++o)c[o].e=e=!e;for(var s,f,l=a[0];;){for(var h=l,p=!0;h.v;)if((h=h.n)===l)return;s=h.z,i.lineStart();do{if(h.v=h.o.v=!0,h.e){if(p)for(o=0,u=s.length;o<u;++o)i.point((f=s[o])[0],f[1]);else r(h.x,h.n.x,1,i);h=h.n}else{if(p)for(s=h.p.z,o=s.length-1;o>=0;--o)i.point((f=s[o])[0],f[1]);else r(h.x,h.p.x,-1,i);h=h.p}h=h.o,s=h.z,p=!p}while(!h.v);i.lineEnd()}}},yb=1e9,gb=-yb,mb=function(){var t,n,e,r=0,i=0,o=960,u=500;return e={stream:function(e){return t&&n===e?t:t=mc(r,i,o,u)(n=e)},extent:function(a){return arguments.length?(r=+a[0][0],i=+a[0][1],o=+a[1][0],u=+a[1][1],t=n=null,e):[[r,i],[o,u]]}}},xb=Ym(),bb={sphere:Ea,point:Ea,lineStart:xc,lineEnd:Ea,polygonStart:Ea,polygonEnd:Ea},wb=function(t){return xb.reset(),Bx(t,bb),+xb},Mb=[null,null],Tb={type:"LineString",coordinates:Mb},Nb=function(t,n){return Mb[0]=t,Mb[1]=n,wb(Tb)},kb=function(){function t(){return{type:"MultiLineString",coordinates:n()}}function n(){return Ls(Px(o/_)*_,i,_).map(h).concat(Ls(Px(s/y)*y,c,y).map(p)).concat(Ls(Px(r/d)*d,e,d).filter(function(t){return Ax(t%_)>xx}).map(f)).concat(Ls(Px(a/v)*v,u,v).filter(function(t){return Ax(t%y)>xx}).map(l))}var e,r,i,o,u,a,c,s,f,l,h,p,d=10,v=d,_=90,y=360,g=2.5;return t.lines=function(){return n().map(function(t){return{type:"LineString",coordinates:t}})},t.outline=function(){return{type:"Polygon",coordinates:[h(o).concat(p(c).slice(1),h(i).reverse().slice(1),p(s).reverse().slice(1))]}},t.extent=function(n){return arguments.length?t.extentMajor(n).extentMinor(n):t.extentMinor()},t.extentMajor=function(n){return arguments.length?(o=+n[0][0],i=+n[1][0],s=+n[0][1],c=+n[1][1],o>i&&(n=o,o=i,i=n),s>c&&(n=s,s=c,c=n),t.precision(g)):[[o,s],[i,c]]},t.extentMinor=function(n){return arguments.length?(r=+n[0][0],e=+n[1][0],a=+n[0][1],u=+n[1][1],r>e&&(n=r,r=e,e=n),a>u&&(n=a,a=u,u=n),t.precision(g)):[[r,a],[e,u]]},t.step=function(n){return arguments.length?t.stepMajor(n).stepMinor(n):t.stepMinor()},t.stepMajor=function(n){return arguments.length?(_=+n[0],y=+n[1],t):[_,y]},t.stepMinor=function(n){return arguments.length?(d=+n[0],v=+n[1],t):[d,v]},t.precision=function(n){return arguments.length?(g=+n,f=Tc(a,u,90),l=Nc(r,e,g),h=Tc(s,c,90),p=Nc(o,i,g),t):g},t.extentMajor([[-180,-90+xx],[180,90-xx]]).extentMinor([[-180,-80-xx],[180,80+xx]])},Sb=function(t,n){var e=t[0]*Sx,r=t[1]*Sx,i=n[0]*Sx,o=n[1]*Sx,u=zx(r),a=Ux(r),c=zx(o),s=Ux(o),f=u*zx(e),l=u*Ux(e),h=c*zx(i),p=c*Ux(i),d=2*Sa(Ox(Aa(o-r)+u*c*Aa(i-e))),v=Ux(d),_=d?function(t){var n=Ux(t*=d)/v,e=Ux(d-t)/v,r=e*f+n*h,i=e*l+n*p,o=e*a+n*s;return[Cx(i,r)*kx,Cx(o,Ox(r*r+i*i))*kx]}:function(){return[e*kx,r*kx]};return _.distance=d,_},Ab=function(t){return t},Eb=Ym(),Cb=Ym(),zb={point:Ea,lineStart:Ea,lineEnd:Ea,polygonStart:function(){zb.lineStart=kc,zb.lineEnd=Ec},polygonEnd:function(){zb.lineStart=zb.lineEnd=zb.point=Ea,Eb.add(Ax(Cb)),Cb.reset()},result:function(){var t=Eb/2;return Eb.reset(),t}},Pb=1/0,qb=Pb,Lb=-Pb,Rb=Lb,Ub={point:Cc,lineStart:Ea,lineEnd:Ea,polygonStart:Ea,polygonEnd:Ea,result:function(){var t=[[Pb,qb],[Lb,Rb]];return Lb=Rb=-(qb=Pb=1/0),t}},Db=0,Ob=0,Fb=0,Ib=0,Yb=0,Bb=0,jb=0,Hb=0,Xb=0,Vb={point:zc,lineStart:Pc,lineEnd:Rc,polygonStart:function(){Vb.lineStart=Uc,Vb.lineEnd=Dc},polygonEnd:function(){Vb.point=zc,Vb.lineStart=Pc,Vb.lineEnd=Rc},result:function(){var t=Xb?[jb/Xb,Hb/Xb]:Bb?[Ib/Bb,Yb/Bb]:Fb?[Db/Fb,Ob/Fb]:[NaN,NaN];return Db=Ob=Fb=Ib=Yb=Bb=jb=Hb=Xb=0,t}};Ic.prototype={_radius:4.5,pointRadius:function(t){return this._radius=t,this},polygonStart:function(){this._line=0},polygonEnd:function(){this._line=NaN},lineStart:function(){this._point=0},lineEnd:function(){0===this._line&&this._context.closePath(),this._point=NaN},point:function(t,n){switch(this._point){case 0:this._context.moveTo(t,n),this._point=1;break;case 1:this._context.lineTo(t,n);break;default:this._context.moveTo(t+this._radius,n),this._context.arc(t,n,this._radius,0,Nx)}},result:Ea},Yc.prototype={_circle:Bc(4.5),pointRadius:function(t){return this._circle=Bc(t),this},polygonStart:function(){this._line=0},polygonEnd:function(){this._line=NaN},lineStart:function(){this._point=0},lineEnd:function(){0===this._line&&this._string.push("Z"),this._point=NaN},point:function(t,n){switch(this._point){case 0:this._string.push("M",t,",",n),this._point=1;break;case 1:this._string.push("L",t,",",n);break;default:this._string.push("M",t,",",n,this._circle)}},result:function(){if(this._string.length){var t=this._string.join("");return this._string=[],t}}};var Wb=function(){function t(t){return t&&("function"==typeof o&&i.pointRadius(+o.apply(this,arguments)),Bx(t,e(i))),i.result()}var n,e,r,i,o=4.5;return t.area=function(t){return Bx(t,e(zb)),zb.result()},t.bounds=function(t){return Bx(t,e(Ub)),Ub.result()},t.centroid=function(t){return Bx(t,e(Vb)),Vb.result()},t.projection=function(r){return arguments.length?(e=null==(n=r)?Ab:r.stream,t):n},t.context=function(n){return arguments.length?(i=null==(r=n)?new Yc:new Ic(n),"function"!=typeof o&&i.pointRadius(o),t):r},t.pointRadius=function(n){return arguments.length?(o="function"==typeof n?n:(i.pointRadius(+n),+n),t):o},t.projection(null).context(null)},$b=Ym(),Zb=function(t,n){var e=n[0],r=n[1],i=[Ux(e),-zx(e),0],o=0,u=0;$b.reset();for(var a=0,c=t.length;a<c;++a)if(f=(s=t[a]).length)for(var s,f,l=s[f-1],h=l[0],p=l[1]/2+Tx,d=Ux(p),v=zx(p),_=0;_<f;++_,h=g,d=x,v=b,l=y){var y=s[_],g=y[0],m=y[1]/2+Tx,x=Ux(m),b=zx(m),w=g-h,M=w>=0?1:-1,T=M*w,N=T>wx,k=d*x;if($b.add(Cx(k*M*Ux(T),v*b+k*zx(T))),o+=N?w+M*Nx:w,N^h>=e^g>=e){var S=Ia(Oa(l),Oa(y));ja(S);var A=Ia(i,S);ja(A);var E=(N^w>=0?-1:1)*Sa(A[2]);(r>E||r===E&&(S[0]||S[1]))&&(u+=N^w>=0?1:-1)}}return(o<-xx||o<xx&&$b<-xx)^1&u},Gb=function(t,n,e,r){return function(i,o){function u(n,e){var r=i(n,e);t(n=r[0],e=r[1])&&o.point(n,e)}function a(t,n){var e=i(t,n);_.point(e[0],e[1])}function c(){b.point=a,_.lineStart()}function s(){b.point=u,_.lineEnd()}function f(t,n){v.push([t,n]);var e=i(t,n);m.point(e[0],e[1])}function l(){m.lineStart(),v=[]}function h(){f(v[0][0],v[0][1]),m.lineEnd();var t,n,e,r,i=m.clean(),u=g.result(),a=u.length;if(v.pop(),p.push(v),v=null,a)if(1&i){if(e=u[0],(n=e.length-1)>0){for(x||(o.polygonStart(),x=!0),o.lineStart(),t=0;t<n;++t)o.point((r=e[t])[0],r[1]);o.lineEnd()}}else a>1&&2&i&&u.push(u.pop().concat(u.shift())),d.push(u.filter(jc))}var p,d,v,_=n(o),y=i.invert(r[0],r[1]),g=pb(),m=n(g),x=!1,b={point:u,lineStart:c,lineEnd:s,polygonStart:function(){b.point=f,b.lineStart=l,b.lineEnd=h,d=[],p=[]},polygonEnd:function(){b.point=u,b.lineStart=c,b.lineEnd=s,d=Ws(d);var t=Zb(p,y);d.length?(x||(o.polygonStart(),x=!0),_b(d,Hc,t,e,o)):t&&(x||(o.polygonStart(),x=!0),o.lineStart(),e(null,null,1,o),o.lineEnd()),x&&(o.polygonEnd(),x=!1),d=p=null},sphere:function(){o.polygonStart(),o.lineStart(),e(null,null,1,o),o.lineEnd(),o.polygonEnd()}};return b}},Jb=Gb(function(){return!0},Xc,Wc,[-wx,-Mx]),Qb=function(t,n){function e(e,r,i,o){vc(o,t,n,i,e,r)}function r(t,n){return zx(t)*zx(n)>a}function i(t){var n,e,i,a,f;return{lineStart:function(){a=i=!1,f=1},point:function(l,h){var p,d=[l,h],v=r(l,h),_=c?v?0:u(l,h):v?u(l+(l<0?wx:-wx),h):0;if(!n&&(a=i=v)&&t.lineStart(),v!==i&&(p=o(n,d),(vb(n,p)||vb(d,p))&&(d[0]+=xx,d[1]+=xx,v=r(d[0],d[1]))),v!==i)f=0,v?(t.lineStart(),p=o(d,n),t.point(p[0],p[1])):(p=o(n,d),t.point(p[0],p[1]),t.lineEnd()),n=p;else if(s&&n&&c^v){var y;_&e||!(y=o(d,n,!0))||(f=0,c?(t.lineStart(),t.point(y[0][0],y[0][1]),t.point(y[1][0],y[1][1]),t.lineEnd()):(t.point(y[1][0],y[1][1]),t.lineEnd(),t.lineStart(),t.point(y[0][0],y[0][1])))}!v||n&&vb(n,d)||t.point(d[0],d[1]),n=d,i=v,e=_},lineEnd:function(){i&&t.lineEnd(),n=null},clean:function(){return f|(a&&i)<<1}}}function o(t,n,e){var r=Oa(t),i=Oa(n),o=[1,0,0],u=Ia(r,i),c=Fa(u,u),s=u[0],f=c-s*s;if(!f)return!e&&t;var l=a*c/f,h=-a*s/f,p=Ia(o,u),d=Ba(o,l),v=Ba(u,h);Ya(d,v);var _=p,y=Fa(d,_),g=Fa(_,_),m=y*y-g*(Fa(d,d)-1);if(!(m<0)){var x=Ox(m),b=Ba(_,(-y-x)/g);if(Ya(b,d),b=Da(b),!e)return b;var w,M=t[0],T=n[0],N=t[1],k=n[1];T<M&&(w=M,M=T,T=w);var S=T-M,A=Ax(S-wx)<xx,E=A||S<xx;if(!A&&k<N&&(w=N,N=k,k=w),E?A?N+k>0^b[1]<(Ax(b[0]-M)<xx?N:k):N<=b[1]&&b[1]<=k:S>wx^(M<=b[0]&&b[0]<=T)){var C=Ba(_,(-y+x)/g);return Ya(C,d),[b,Da(C)]}}}function u(n,e){var r=c?t:wx-t,i=0;return n<-r?i|=1:n>r&&(i|=2),e<-r?i|=4:e>r&&(i|=8),i}var a=zx(t),c=a>0,s=Ax(a)>xx;return Gb(r,i,e,c?[0,-t]:[-wx,t-wx])},Kb=function(t){return{stream:$c(t)}};Zc.prototype={point:function(t,n){this.stream.point(t,n)},sphere:function(){this.stream.sphere()},lineStart:function(){this.stream.lineStart()},lineEnd:function(){this.stream.lineEnd()},polygonStart:function(){this.stream.polygonStart()},polygonEnd:function(){this.stream.polygonEnd()}};var tw=16,nw=zx(30*Sx),ew=function(t,n){return+n?ts(t,n):Kc(t)},rw=$c({point:function(t,n){this.stream.point(t*Sx,n*Sx)}}),iw=function(){return rs(is).scale(155.424).center([0,33.6442])},ow=function(){return iw().parallels([29.5,45.5]).scale(1070).translate([480,250]).rotate([96,0]).center([-.6,38.7])},uw=function(){function t(t){var n=t[0],e=t[1];return a=null,i.point(n,e),a||(o.point(n,e),a)||(u.point(n,e),a)}function n(){return e=r=null,t}var e,r,i,o,u,a,c=ow(),s=iw().rotate([154,0]).center([-2,58.5]).parallels([55,65]),f=iw().rotate([157,0]).center([-3,19.9]).parallels([8,18]),l={point:function(t,n){a=[t,n]}};return t.invert=function(t){var n=c.scale(),e=c.translate(),r=(t[0]-e[0])/n,i=(t[1]-e[1])/n;return(i>=.12&&i<.234&&r>=-.425&&r<-.214?s:i>=.166&&i<.234&&r>=-.214&&r<-.115?f:c).invert(t)},t.stream=function(t){return e&&r===t?e:e=os([c.stream(r=t),s.stream(t),f.stream(t)])},t.precision=function(t){return arguments.length?(c.precision(t),s.precision(t),f.precision(t),n()):c.precision()},t.scale=function(n){return arguments.length?(c.scale(n),s.scale(.35*n),f.scale(n),t.translate(c.translate())):c.scale()},t.translate=function(t){if(!arguments.length)return c.translate();var e=c.scale(),r=+t[0],a=+t[1];return i=c.translate(t).clipExtent([[r-.455*e,a-.238*e],[r+.455*e,a+.238*e]]).stream(l),o=s.translate([r-.307*e,a+.201*e]).clipExtent([[r-.425*e+xx,a+.12*e+xx],[r-.214*e-xx,a+.234*e-xx]]).stream(l),u=f.translate([r-.205*e,a+.212*e]).clipExtent([[r-.214*e+xx,a+.166*e+xx],[r-.115*e-xx,a+.234*e-xx]]).stream(l),n()},t.fitExtent=Qc(t),t.fitSize=Jc(t),t.scale(1070)},aw=us(function(t){return Ox(2/(1+t))});aw.invert=as(function(t){return 2*Sa(t/2)});var cw=function(){return ns(aw).scale(124.75).clipAngle(179.999)},sw=us(function(t){return(t=ka(t))&&t/Ux(t)});sw.invert=as(function(t){return t});var fw=function(){return ns(sw).scale(79.4188).clipAngle(179.999)};cs.invert=function(t,n){return[t,2*Ex(qx(n))-Mx]};var lw=function(){return ss(cs).scale(961/Nx)},hw=function(){return rs(ls).scale(109.5).parallels([30,30])};hs.invert=hs;var pw=function(){return ns(hs).scale(152.63)},dw=function(){return rs(ps).scale(131.154).center([0,13.9389])};ds.invert=as(Ex);var vw=function(){return ns(ds).scale(144.049).clipAngle(60)};vs.invert=as(Sa);var _w=function(){return ns(vs).scale(249.5).clipAngle(90+xx)};_s.invert=as(function(t){return 2*Ex(t)});var yw=function(){return ns(_s).scale(250).clipAngle(142)};ys.invert=function(t,n){return[-n,2*Ex(qx(t))-Mx]};var gw=function(){var t=ss(ys),n=t.center,e=t.rotate;return t.center=function(t){return arguments.length?n([-t[1],t[0]]):(t=n(),[t[1],-t[0]])},t.rotate=function(t){return arguments.length?e([t[0],t[1],t.length>2?t[2]+90:90]):(t=e(),[t[0],t[1],t[2]-90])},e([0,0,90]).scale(159.155)};t.version=gs,t.bisect=ws,t.bisectRight=ws,t.bisectLeft=Ms,t.ascending=ms,t.bisector=xs,t.descending=Ts,t.deviation=Ss,t.extent=As,t.histogram=Is,t.thresholdFreedmanDiaconis=Bs,t.thresholdScott=js,t.thresholdSturges=Fs,t.max=Hs,t.mean=Xs,t.median=Vs,t.merge=Ws,t.min=$s,t.pairs=Zs,t.permute=Gs,t.quantile=Ys,t.range=Ls,t.scan=Js,t.shuffle=Qs,t.sum=Ks,t.ticks=Os,t.tickStep=e,t.transpose=tf,t.variance=ks,t.zip=nf,t.entries=cf,t.keys=uf,t.values=af,t.map=o,t.set=l,t.nest=rf,t.randomUniform=sf,t.randomNormal=ff,t.randomLogNormal=lf,t.randomBates=pf,t.randomIrwinHall=hf,t.randomExponential=df,t.easeLinear=h,t.easeQuad=v,t.easeQuadIn=p,t.easeQuadOut=d,t.easeQuadInOut=v,t.easeCubic=g,t.easeCubicIn=_,t.easeCubicOut=y,t.easeCubicInOut=g,t.easePoly=gf,t.easePolyIn=_f,t.easePolyOut=yf,t.easePolyInOut=gf,t.easeSin=b,t.easeSinIn=m,t.easeSinOut=x,t.easeSinInOut=b,t.easeExp=T,t.easeExpIn=w,t.easeExpOut=M,t.easeExpInOut=T,t.easeCircle=S,t.easeCircleIn=N,t.easeCircleOut=k,t.easeCircleInOut=S,t.easeBounce=E,t.easeBounceIn=A,t.easeBounceOut=E,t.easeBounceInOut=C,t.easeBack=Lf,t.easeBackIn=Pf,t.easeBackOut=qf,t.easeBackInOut=Lf,t.easeElastic=Ff,t.easeElasticIn=Of,t.easeElasticOut=Ff,t.easeElasticInOut=If,t.polygonArea=Yf,t.polygonCentroid=Bf,t.polygonHull=Hf,t.polygonContains=Xf,t.polygonLength=Vf,t.path=L,t.quadtree=I,t.queue=Z,t.arc=gl,t.area=bl,t.line=xl,t.pie=Tl,t.radialArea=Sl,t.radialLine=kl,t.symbol=Wl,t.symbols=Vl,t.symbolCircle=Al,t.symbolCross=El,t.symbolDiamond=Pl,t.symbolSquare=Ol,t.symbolStar=Dl,t.symbolTriangle=Il,t.symbolWye=Xl,t.curveBasisClosed=Gl,t.curveBasisOpen=Jl,t.curveBasis=Zl,t.curveBundle=Ql,t.curveCardinalClosed=th,t.curveCardinalOpen=nh,t.curveCardinal=Kl,t.curveCatmullRomClosed=rh,t.curveCatmullRomOpen=ih,t.curveCatmullRom=eh,t.curveLinearClosed=oh,t.curveLinear=ml,t.curveMonotoneX=zt,t.curveMonotoneY=Pt,t.curveNatural=uh,t.curveStep=ah,t.curveStepAfter=Dt,t.curveStepBefore=Ut,t.stack=lh,t.stackOffsetExpand=hh,t.stackOffsetNone=sh,t.stackOffsetSilhouette=ph,t.stackOffsetWiggle=dh,t.stackOrderAscending=vh,t.stackOrderDescending=_h,t.stackOrderInsideOut=yh,t.stackOrderNone=fh,t.stackOrderReverse=gh,t.color=Bt,t.rgb=Vt,t.hsl=Gt,t.lab=tn,t.hcl=cn,t.cubehelix=ln,t.interpolate=hp,t.interpolateArray=op,t.interpolateDate=up,t.interpolateNumber=ap,t.interpolateObject=cp,t.interpolateRound=pp,t.interpolateString=lp,t.interpolateTransformCss=yp,t.interpolateTransformSvg=gp,t.interpolateZoom=Mp,t.interpolateRgb=ep,t.interpolateRgbBasis=rp,t.interpolateRgbBasisClosed=ip,t.interpolateHsl=Tp,t.interpolateHslLong=Np,t.interpolateLab=En,t.interpolateHcl=kp,t.interpolateHclLong=Sp,t.interpolateCubehelix=Ap,t.interpolateCubehelixLong=Ep,t.interpolateBasis=Kh,t.interpolateBasisClosed=tp,t.quantize=Cp,t.dispatch=Pn,t.dsvFormat=Lp,t.csvParse=Up,t.csvParseRows=Dp,t.csvFormat=Op,t.csvFormatRows=Fp,t.tsvParse=Yp,t.tsvParseRows=Bp,t.tsvFormat=jp,t.tsvFormatRows=Hp,t.request=Xp,t.html=Wp,t.json=$p,t.text=Zp,t.xml=Gp,t.csv=Qp,t.tsv=Kp,t.now=jn,t.timer=Vn,t.timerFlush=Wn,t.timeout=sd,t.interval=fd,t.timeInterval=Qn,t.timeMillisecond=pd,t.timeMilliseconds=dd,t.timeSecond=xd,t.timeSeconds=bd,t.timeMinute=wd,t.timeMinutes=Md,t.timeHour=Td,t.timeHours=Nd,t.timeDay=kd,t.timeDays=Sd,t.timeWeek=Ad,t.timeWeeks=Rd,t.timeSunday=Ad,t.timeSundays=Rd,t.timeMonday=Ed,t.timeMondays=Ud,t.timeTuesday=Cd,t.timeTuesdays=Dd;t.timeWednesday=zd;t.timeWednesdays=Od,t.timeThursday=Pd,t.timeThursdays=Fd,t.timeFriday=qd,t.timeFridays=Id,t.timeSaturday=Ld,t.timeSaturdays=Yd,t.timeMonth=Bd,t.timeMonths=jd,t.timeYear=Hd,t.timeYears=Xd,t.utcMillisecond=pd,t.utcMilliseconds=dd,t.utcSecond=xd,t.utcSeconds=bd,t.utcMinute=Vd,t.utcMinutes=Wd,t.utcHour=$d,t.utcHours=Zd,t.utcDay=Gd,t.utcDays=Jd,t.utcWeek=Qd,t.utcWeeks=ov,t.utcSunday=Qd,t.utcSundays=ov,t.utcMonday=Kd,t.utcMondays=uv,t.utcTuesday=tv,t.utcTuesdays=av,t.utcWednesday=nv,t.utcWednesdays=cv,t.utcThursday=ev,t.utcThursdays=sv,t.utcFriday=rv,t.utcFridays=fv,t.utcSaturday=iv,t.utcSaturdays=lv,t.utcMonth=hv,t.utcMonths=pv,t.utcYear=dv,t.utcYears=_v,t.formatLocale=Av,t.formatDefaultLocale=re,t.formatSpecifier=Nv,t.precisionFixed=Cv,t.precisionPrefix=zv,t.precisionRound=Pv,t.isoFormat=Ov,t.isoParse=Fv,t.timeFormatLocale=ae,t.timeFormatDefaultLocale=nr,t.scaleBand=or,t.scalePoint=ar,t.scaleIdentity=yr,t.scaleLinear=_r,t.scaleLog=Tr,t.scaleOrdinal=ir,t.scaleImplicit=jv,t.scalePow=kr,t.scaleSqrt=Sr,t.scaleQuantile=Ar,t.scaleQuantize=Er,t.scaleThreshold=Cr,t.scaleTime=e_,t.scaleUtc=r_,t.schemeCategory10=o_,t.schemeCategory20b=u_,t.schemeCategory20c=a_,t.schemeCategory20=c_,t.scaleSequential=Rr,t.interpolateCubehelixDefault=s_,t.interpolateRainbow=p_,t.interpolateWarm=f_,t.interpolateCool=l_,t.interpolateViridis=d_,t.interpolateMagma=v_,t.interpolateInferno=__,t.interpolatePlasma=y_,t.creator=b_,t.customEvent=Xr,t.local=Or,t.matcher=k_,t.mouse=P_,t.namespace=x_,t.namespaces=m_,t.select=dy,t.selectAll=vy,t.selection=Pi,t.selector=q_,t.selectorAll=R_,t.touch=_y,t.touches=yy,t.window=ty,t.active=tg,t.interrupt=Ay,t.transition=fo,t.axisTop=mo,t.axisRight=xo,t.axisBottom=bo,t.axisLeft=wo,t.cluster=cg,t.hierarchy=zo,t.pack=Mg,t.packSiblings=bg,t.packEnclose=xg,t.partition=kg,t.stratify=Cg,t.tree=zg,t.treemap=Rg,t.treemapBinary=Ug,t.treemapDice=Ng,t.treemapSlice=Pg,t.treemapSliceDice=Dg,t.treemapSquarify=Lg,t.treemapResquarify=Og,t.forceCenter=Fg,t.forceCollide=Bg,t.forceLink=jg,t.forceManyBody=Wg,t.forceSimulation=Vg,t.forceX=$g,t.forceY=Zg,t.drag=Kg,t.dragDisable=Jg,t.dragEnable=gu,t.voronoi=fm,t.zoom=dm,t.zoomIdentity=hm,t.zoomTransform=ra,t.brush=Cm,t.brushX=va,t.brushY=_a,t.brushSelection=da,t.chord=Dm,t.ribbon=Im,t.geoAlbers=ow,t.geoAlbersUsa=uw,t.geoArea=Vx,t.geoAzimuthalEqualArea=cw,t.geoAzimuthalEqualAreaRaw=aw,t.geoAzimuthalEquidistant=fw,t.geoAzimuthalEquidistantRaw=sw,t.geoBounds=Zx,t.geoCentroid=Jx,t.geoCircle=hb,t.geoClipExtent=mb,t.geoConicConformal=hw,t.geoConicConformalRaw=ls,t.geoConicEqualArea=iw,t.geoConicEqualAreaRaw=is,t.geoConicEquidistant=dw,t.geoConicEquidistantRaw=ps,t.geoDistance=Nb,t.geoEquirectangular=pw,t.geoEquirectangularRaw=hs,t.geoGnomonic=vw,t.geoGnomonicRaw=ds,t.geoGraticule=kb,t.geoInterpolate=Sb,t.geoLength=wb,t.geoMercator=lw,t.geoMercatorRaw=cs,t.geoOrthographic=_w,t.geoOrthographicRaw=vs,t.geoPath=Wb,t.geoProjection=ns,t.geoProjectionMutator=es,t.geoRotation=lb,t.geoStereographic=yw,t.geoStereographicRaw=_s,t.geoStream=Bx,t.geoTransform=Kb,t.geoTransverseMercator=gw,t.geoTransverseMercatorRaw=ys,Object.defineProperty(t,"__esModule",{value:!0})});

 
// https://github.com/topojson/topojson Version 2.2.0. Copyright 2016 Mike Bostock.
(function(n,r){"object"==typeof exports&&"undefined"!=typeof module?r(exports):"function"==typeof define&&define.amd?define(["exports"],r):r(n.topojson=n.topojson||{})})(this,function(n){"use strict";function r(n,r,e,o){t(n,r,e),t(n,r,r+o),t(n,r+o,e)}function t(n,r,t){for(var e,o=r+(t-- -r>>1);r<o;++r,--t)e=n[r],n[r]=n[t],n[t]=e}function e(n){return(n&&U.hasOwnProperty(n.type)?U[n.type]:a)(n)}function o(n){var r=n.geometry;return null==r?n.type=null:(a(r),n.type=r.type,r.geometries?n.geometries=r.geometries:r.coordinates&&(n.coordinates=r.coordinates),r.bbox&&(n.bbox=r.bbox)),delete n.geometry,n}function a(n){return n?(z.hasOwnProperty(n.type)&&z[n.type](n),n):{type:null}}function i(n){var r,t=n[0],e=n[1];return e<t&&(r=t,t=e,e=r),t+31*e}function c(n,r){var t,e=n[0],o=n[1],a=r[0],i=r[1];return o<e&&(t=e,e=o,o=t),i<a&&(t=a,a=i,i=t),e===a&&o===i}function u(){return!0}function f(n){return n}function l(n){return null!=n.type}function s(n,r){var t=r.id,e=r.bbox,o=null==r.properties?{}:r.properties,a=h(n,r);return null==t&&null==e?{type:"Feature",properties:o,geometry:a}:null==e?{type:"Feature",id:t,properties:o,geometry:a}:{type:"Feature",id:t,bbox:e,properties:o,geometry:a}}function h(n,r){function t(n,r){r.length&&r.pop();for(var t=f[n<0?~n:n],e=0,o=t.length;e<o;++e)r.push(u(t[e].slice(),e));n<0&&Q(r,o)}function e(n){return u(n.slice())}function o(n){for(var r=[],e=0,o=n.length;e<o;++e)t(n[e],r);return r.length<2&&r.push(r[0].slice()),r}function a(n){for(var r=o(n);r.length<4;)r.push(r[0].slice());return r}function i(n){return n.map(a)}function c(n){var r,t=n.type;switch(t){case"GeometryCollection":return{type:t,geometries:n.geometries.map(c)};case"Point":r=e(n.coordinates);break;case"MultiPoint":r=n.coordinates.map(e);break;case"LineString":r=o(n.arcs);break;case"MultiLineString":r=n.arcs.map(o);break;case"Polygon":r=i(n.arcs);break;case"MultiPolygon":r=n.arcs.map(i);break;default:return null}return{type:t,coordinates:r}}var u=J(n),f=n.arcs;return c(r)}function g(n,r,t){var e,o,a;if(arguments.length>1)e=v(n,r,t);else for(o=0,e=new Array(a=n.arcs.length);o<a;++o)e[o]=o;return{type:"MultiLineString",arcs:Y(n,e)}}function v(n,r,t){function e(n){var r=n<0?~n:n;(l[r]||(l[r]=[])).push({i:n,g:u})}function o(n){n.forEach(e)}function a(n){n.forEach(o)}function i(n){n.forEach(a)}function c(n){switch(u=n,n.type){case"GeometryCollection":n.geometries.forEach(c);break;case"LineString":o(n.arcs);break;case"MultiLineString":case"Polygon":a(n.arcs);break;case"MultiPolygon":i(n.arcs)}}var u,f=[],l=[];return c(r),l.forEach(null==t?function(n){f.push(n[0].i)}:function(n){t(n[0].g,n[n.length-1].g)&&f.push(n[0].i)}),f}function p(n){for(var r,t=-1,e=n.length,o=n[e-1],a=0;++t<e;)r=o,o=n[t],a+=r[0]*o[1]-r[1]*o[0];return Math.abs(a)}function y(n,r){function t(n){switch(n.type){case"GeometryCollection":n.geometries.forEach(t);break;case"Polygon":e(n.arcs);break;case"MultiPolygon":n.arcs.forEach(e)}}function e(n){n.forEach(function(r){r.forEach(function(r){(a[r=r<0?~r:r]||(a[r]=[])).push(n)})}),i.push(n)}function o(r){return p(h(n,{type:"Polygon",arcs:[r]}).coordinates[0])}var a={},i=[],c=[];return r.forEach(t),i.forEach(function(n){if(!n._){var r=[],t=[n];for(n._=1,c.push(r);n=t.pop();)r.push(n),n.forEach(function(n){n.forEach(function(n){a[n<0?~n:n].forEach(function(n){n._||(n._=1,t.push(n))})})})}}),i.forEach(function(n){delete n._}),{type:"MultiPolygon",arcs:c.map(function(r){var t,e=[];if(r.forEach(function(n){n.forEach(function(n){n.forEach(function(n){a[n<0?~n:n].length<2&&e.push(n)})})}),e=Y(n,e),(t=e.length)>1)for(var i,c,u=1,f=o(e[0]);u<t;++u)(i=o(e[u]))>f&&(c=e[0],e[0]=e[u],e[u]=c,f=i);return e})}}function d(n){var r=n[0],t=n[1],e=n[2];return Math.abs((r[0]-e[0])*(t[1]-r[1])-(r[0]-t[0])*(e[1]-r[1]))}function m(n){for(var r,t=-1,e=n.length,o=n[e-1],a=0;++t<e;)r=o,o=n[t],a+=r[0]*o[1]-r[1]*o[0];return Math.abs(a)/2}function b(n,r){return n[1][2]-r[1][2]}function E(n,r){if(t=n.length){if((r=+r)<=0||t<2)return n[0];if(r>=1)return n[t-1];var t,e=(t-1)*r,o=Math.floor(e),a=n[o],i=n[o+1];return a+(i-a)*(e-o)}}function M(n,r){return r-n}function P(n,r){if(!n.length)return 0;var t,e,o,a,i,c,u,f=0,l=n[0],s=l[0]*gn,h=(l[1]*gn+sn)/2,g=dn(h),v=bn(h);for(i=1,c=n.length;i<c;++i)l=n[i],t=s,s=l[0]*gn,e=s-t,h=(l[1]*gn+sn)/2,o=g,g=dn(h),a=v,v=bn(h),u=a*v,f+=yn(u*bn(e),o*g+u*dn(e));return f=2*(f>ln?f-sn:f<-ln?f+sn:f),r&&(f*=-1),f<0?f+hn:f}function w(n){var r=n[0][0]*gn,t=n[0][1]*gn,e=dn(t),o=bn(t),a=n[1][0]*gn,i=n[1][1]*gn,c=dn(i),u=bn(i),f=n[2][0]*gn,l=n[2][1]*gn,s=dn(l),h=bn(l),g=k(r,e,o,a,c,u),v=k(a,c,u,f,s,h),p=k(f,s,h,r,e,o),y=(g+v+p)/2;return 4*pn(En(mn(0,Mn(y/2)*Mn((y-g)/2)*Mn((y-v)/2)*Mn((y-p)/2))))}function k(n,r,t,e,o,a){var i=vn(e-n),c=dn(i),u=bn(i),f=a*u,l=t*o-r*a*c,s=r*o+t*a*c;return yn(En(f*f+l*l),s)}var x=function(n){function r(n){n&&f.hasOwnProperty(n.type)&&f[n.type](n)}function t(n){var r=n[0],t=n[1];r<a&&(a=r),r>c&&(c=r),t<i&&(i=t),t>u&&(u=t)}function e(n){n.forEach(t)}function o(n){n.forEach(e)}var a=1/0,i=1/0,c=-(1/0),u=-(1/0),f={GeometryCollection:function(n){n.geometries.forEach(r)},Point:function(n){t(n.coordinates)},MultiPoint:function(n){n.coordinates.forEach(t)},LineString:function(n){e(n.coordinates)},MultiLineString:function(n){n.coordinates.forEach(e)},Polygon:function(n){n.coordinates.forEach(e)},MultiPolygon:function(n){n.coordinates.forEach(o)}};for(var l in n)r(n[l]);return c>=a&&u>=i?[a,i,c,u]:void 0},A=function(n,r,t,e,o){function a(e){for(var a=r(e)&f,i=u[a],c=0;i!=o;){if(t(i,e))return!0;if(++c>=n)throw new Error("full hashset");i=u[a=a+1&f]}return u[a]=e,!0}function i(e){for(var a=r(e)&f,i=u[a],c=0;i!=o;){if(t(i,e))return!0;if(++c>=n)break;i=u[a=a+1&f]}return!1}function c(){for(var n=[],r=0,t=u.length;r<t;++r){var e=u[r];e!=o&&n.push(e)}return n}3===arguments.length&&(e=Array,o=null);for(var u=new e(n=1<<Math.max(4,Math.ceil(Math.log(n)/Math.LN2))),f=n-1,l=0;l<n;++l)u[l]=o;return{add:a,has:i,values:c}},L=function(n,r,t,e,o,a){function i(e,a){for(var i=r(e)&h,c=l[i],u=0;c!=o;){if(t(c,e))return s[i]=a;if(++u>=n)throw new Error("full hashmap");c=l[i=i+1&h]}return l[i]=e,s[i]=a,a}function c(e,a){for(var i=r(e)&h,c=l[i],u=0;c!=o;){if(t(c,e))return s[i];if(++u>=n)throw new Error("full hashmap");c=l[i=i+1&h]}return l[i]=e,s[i]=a,a}function u(e,a){for(var i=r(e)&h,c=l[i],u=0;c!=o;){if(t(c,e))return s[i];if(++u>=n)break;c=l[i=i+1&h]}return a}function f(){for(var n=[],r=0,t=l.length;r<t;++r){var e=l[r];e!=o&&n.push(e)}return n}3===arguments.length&&(e=a=Array,o=null);for(var l=new e(n=1<<Math.max(4,Math.ceil(Math.log(n)/Math.LN2))),s=new a(n),h=n-1,g=0;g<n;++g)l[g]=o;return{set:i,maybeSet:c,get:u,keys:f}},S=function(n,r){return n[0]===r[0]&&n[1]===r[1]},C=new ArrayBuffer(16),_=new Float64Array(C),j=new Uint32Array(C),G=function(n){_[0]=n[0],_[1]=n[1];var r=j[0]^j[1];return r=r<<5^r>>7^j[2]^j[3],2147483647&r},I=function(n){function r(n,r,t,e){if(v[t]!==n){v[t]=n;var o=p[t];if(o>=0){var a=y[t];o===r&&a===e||o===e&&a===r||(++m,d[t]=1)}else p[t]=r,y[t]=e}}function t(){for(var n=L(1.4*l.length,e,o,Int32Array,-1,Int32Array),r=new Int32Array(l.length),t=0,a=l.length;t<a;++t)r[t]=n.maybeSet(t,t);return r}function e(n){return G(l[n])}function o(n,r){return S(l[n],l[r])}var a,i,c,u,f,l=n.coordinates,s=n.lines,h=n.rings,g=t(),v=new Int32Array(l.length),p=new Int32Array(l.length),y=new Int32Array(l.length),d=new Int8Array(l.length),m=0;for(a=0,i=l.length;a<i;++a)v[a]=p[a]=y[a]=-1;for(a=0,i=s.length;a<i;++a){var b=s[a],E=b[0],M=b[1];for(u=g[E],f=g[++E],++m,d[u]=1;++E<=M;)r(a,c=u,u=f,f=g[E]);++m,d[f]=1}for(a=0,i=l.length;a<i;++a)v[a]=-1;for(a=0,i=h.length;a<i;++a){var P=h[a],w=P[0]+1,k=P[1];for(c=g[k-1],u=g[w-1],f=g[w],r(a,c,u,f);++w<=k;)r(a,c=u,u=f,f=g[w])}v=p=y=null;var x,C=A(1.4*m,G,S);for(a=0,i=l.length;a<i;++a)d[x=g[a]]&&C.add(l[x]);return C},F=function(n){var t,e,o,a=I(n),i=n.coordinates,c=n.lines,u=n.rings;for(e=0,o=c.length;e<o;++e)for(var f=c[e],l=f[0],s=f[1];++l<s;)a.has(i[l])&&(t={0:l,1:f[1]},f[1]=l,f=f.next=t);for(e=0,o=u.length;e<o;++e)for(var h=u[e],g=h[0],v=g,p=h[1],y=a.has(i[g]);++v<p;)a.has(i[v])&&(y?(t={0:v,1:h[1]},h[1]=v,h=h.next=t):(r(i,g,p,p-v),i[p]=i[g],y=!0,v=g));return n},O=function(n){function r(n){var r,t,a,i,c,u,f,l;if(a=y.get(r=h[n[0]]))for(f=0,l=a.length;f<l;++f)if(i=a[f],e(i,n))return n[0]=i[0],void(n[1]=i[1]);if(c=y.get(t=h[n[1]]))for(f=0,l=c.length;f<l;++f)if(u=c[f],o(u,n))return n[1]=u[0],void(n[0]=u[1]);a?a.push(n):y.set(r,[n]),c?c.push(n):y.set(t,[n]),d.push(n)}function t(n){var r,t,e,o,u;if(t=y.get(r=h[n[0]]))for(o=0,u=t.length;o<u;++o){if(e=t[o],a(e,n))return n[0]=e[0],void(n[1]=e[1]);if(i(e,n))return n[0]=e[1],void(n[1]=e[0])}if(t=y.get(r=h[n[0]+c(n)]))for(o=0,u=t.length;o<u;++o){if(e=t[o],a(e,n))return n[0]=e[0],void(n[1]=e[1]);if(i(e,n))return n[0]=e[1],void(n[1]=e[0])}t?t.push(n):y.set(r,[n]),d.push(n)}function e(n,r){var t=n[0],e=r[0],o=n[1],a=r[1];if(t-o!==e-a)return!1;for(;t<=o;++t,++e)if(!S(h[t],h[e]))return!1;return!0}function o(n,r){var t=n[0],e=r[0],o=n[1],a=r[1];if(t-o!==e-a)return!1;for(;t<=o;++t,--a)if(!S(h[t],h[a]))return!1;return!0}function a(n,r){var t=n[0],e=r[0],o=n[1],a=r[1],i=o-t;if(i!==a-e)return!1;for(var u=c(n),f=c(r),l=0;l<i;++l)if(!S(h[t+(l+u)%i],h[e+(l+f)%i]))return!1;return!0}function i(n,r){var t=n[0],e=r[0],o=n[1],a=r[1],i=o-t;if(i!==a-e)return!1;for(var u=c(n),f=i-c(r),l=0;l<i;++l)if(!S(h[t+(l+u)%i],h[a-(l+f)%i]))return!1;return!0}function c(n){for(var r=n[0],t=n[1],e=r,o=e,a=h[e];++e<t;){var i=h[e];(i[0]<a[0]||i[0]===a[0]&&i[1]<a[1])&&(o=e,a=i)}return o-r}var u,f,l,s,h=n.coordinates,g=n.lines,v=n.rings,p=g.length+v.length;for(delete n.lines,delete n.rings,l=0,s=g.length;l<s;++l)for(u=g[l];u=u.next;)++p;for(l=0,s=v.length;l<s;++l)for(f=v[l];f=f.next;)++p;var y=L(2*p*1.4,G,S),d=n.arcs=[];for(l=0,s=g.length;l<s;++l){u=g[l];do r(u);while(u=u.next)}for(l=0,s=v.length;l<s;++l)if(f=v[l],f.next){do r(f);while(f=f.next)}else t(f);return n},N=function(n){for(var r=n.arcs,t=-1,e=r.length;++t<e;)for(var o,a,i=r[t],c=0,u=i.length,f=i[0],l=f[0],s=f[1];++c<u;)f=i[c],o=f[0],a=f[1],i[c]=[o-l,a-s],l=o,s=a;return n},q=function(n){function r(n){n&&f.hasOwnProperty(n.type)&&f[n.type](n)}function t(n){for(var r=0,t=n.length;r<t;++r)u[++a]=n[r];var e={0:a-t+1,1:a};return i.push(e),e}function e(n){for(var r=0,t=n.length;r<t;++r)u[++a]=n[r];var e={0:a-t+1,1:a};return c.push(e),e}function o(n){return n.map(e)}var a=-1,i=[],c=[],u=[],f={GeometryCollection:function(n){n.geometries.forEach(r)},LineString:function(n){n.arcs=t(n.coordinates),delete n.coordinates},MultiLineString:function(n){n.arcs=n.coordinates.map(t),delete n.coordinates},Polygon:function(n){n.arcs=n.coordinates.map(e),delete n.coordinates},MultiPolygon:function(n){n.arcs=n.coordinates.map(o),delete n.coordinates}};for(var l in n)r(n[l]);return{type:"Topology",coordinates:u,lines:i,rings:c,objects:n}},T=function(n){var r;for(r in n)n[r]=e(n[r]);return n},U={Feature:o,FeatureCollection:function(n){return n.type="GeometryCollection",n.geometries=n.features,n.features.forEach(o),delete n.features,n}},z={GeometryCollection:function(n){for(var r=n.geometries,t=-1,e=r.length;++t<e;)r[t]=a(r[t])},MultiPoint:function(n){n.coordinates.length?n.coordinates.length<2&&(n.type="Point",n.coordinates=n.coordinates[0]):(n.type=null,delete n.coordinates)},LineString:function(n){n.coordinates.length||(n.type=null,delete n.coordinates)},MultiLineString:function(n){for(var r=n.coordinates,t=0,e=0,o=r.length;t<o;++t){var a=r[t];a.length&&(r[e++]=a)}e?e<2?(n.type="LineString",n.coordinates=r[0]):n.coordinates.length=e:(n.type=null,delete n.coordinates)},Polygon:function(n){for(var r=n.coordinates,t=0,e=0,o=r.length;t<o;++t){var a=r[t];a.length&&(r[e++]=a)}e?n.coordinates.length=e:(n.type=null,delete n.coordinates)},MultiPolygon:function(n){for(var r=n.coordinates,t=0,e=0,o=r.length;t<o;++t){for(var a=r[t],i=0,c=0,u=a.length;i<u;++i){var f=a[i];f.length&&(a[c++]=f)}c&&(a.length=c,r[e++]=a)}e?e<2?(n.type="Polygon",n.coordinates=r[0]):r.length=e:(n.type=null,delete n.coordinates)}},R=function(n,r,t){function e(n){return n[0]=Math.round((n[0]-i)*l),n[1]=Math.round((n[1]-c)*s),n}function o(n){for(var r,t,o,a=0,i=1,c=n.length,u=e(n[0]),f=u[0],l=u[1];++a<c;)u=e(n[a]),t=u[0],o=u[1],t===f&&o===l||(r=n[i++],r[0]=f=t,r[1]=l=o);n.length=i}function a(n){n&&h.hasOwnProperty(n.type)&&h[n.type](n)}var i=r[0],c=r[1],u=r[2],f=r[3],l=u-i?(t-1)/(u-i):1,s=f-c?(t-1)/(f-c):1,h={GeometryCollection:function(n){n.geometries.forEach(a)},Point:function(n){e(n.coordinates)},MultiPoint:function(n){n.coordinates.forEach(e)},LineString:function(n){var r=n.coordinates;o(r),r.length<2&&(r[1]=r[0])},MultiLineString:function(n){for(var r=n.coordinates,t=0,e=r.length;t<e;++t){var a=r[t];o(a),a.length<2&&(a[1]=a[0])}},Polygon:function(n){for(var r=n.coordinates,t=0,e=r.length;t<e;++t){var a=r[t];for(o(a);a.length<4;)a.push(a[0])}},MultiPolygon:function(n){for(var r=n.coordinates,t=0,e=r.length;t<e;++t)for(var a=r[t],i=0,c=a.length;i<c;++i){var u=a[i];for(o(u);u.length<4;)u.push(u[0])}}};for(var g in n)a(n[g]);return{scale:[1/l,1/s],translate:[i,c]}},V=function(n,r){function t(n){n&&h.hasOwnProperty(n.type)&&h[n.type](n)}function e(n){var r=[];do{var t=s.get(n);r.push(n[0]<n[1]?t:~t)}while(n=n.next);return r}function o(n){return n.map(e)}var a=x(T(n)),u=r>0&&a&&R(n,a,r),f=O(F(q(n))),l=f.coordinates,s=L(1.4*f.arcs.length,i,c);n=f.objects,f.bbox=a,f.arcs=f.arcs.map(function(n,r){return s.set(n,r),l.slice(n[0],n[1]+1)}),delete f.coordinates,l=null;var h={GeometryCollection:function(n){n.geometries.forEach(t)},LineString:function(n){n.arcs=e(n.arcs)},MultiLineString:function(n){n.arcs=n.arcs.map(e)},Polygon:function(n){n.arcs=n.arcs.map(e)},MultiPolygon:function(n){n.arcs=n.arcs.map(o)}};for(var g in n)t(n[g]);return u&&(f.transform=u,N(f)),f},B=function(n){function r(n){switch(n.type){case"GeometryCollection":n.geometries.forEach(r);break;case"LineString":t(n.arcs);break;case"MultiLineString":n.arcs.forEach(t);break;case"Polygon":n.arcs.forEach(t);break;case"MultiPolygon":n.arcs.forEach(e)}}function t(n){for(var r=0,t=n.length;r<t;++r){var e,o=n[r],f=o<0&&(o=~o,!0);null==(e=u[o])&&(u[o]=e=++c,i[e]=a[o]),n[r]=f?~e:e}}function e(n){n.forEach(t)}var o,a=n.arcs,i=n.arcs=[],c=-1,u=new Array(a.length);for(o in n.objects)r(n.objects[o]);return n},W=function(n,r){function t(n){switch(n.type){case"Polygon":n.arcs=e(n.arcs),n.arcs||(n.type=null,delete n.arcs);break;case"MultiPolygon":n.arcs=n.arcs.map(e).filter(f),n.arcs.length||(n.type=null,delete n.arcs);break;case"GeometryCollection":n.geometries.forEach(t),n.geometries=n.geometries.filter(l),n.geometries.length||(n.type=null,delete n.geometries)}}function e(n){return n.length&&o(n[0])?[n.shift()].concat(n.filter(a)):null}function o(n){return r(n,!1)}function a(n){return r(n,!0)}var i;null==r&&(r=u);for(i in n.objects)t(n.objects[i]);return B(n)},D=function(n){function r(n){switch(n.type){case"GeometryCollection":n.geometries.forEach(r);break;case"Polygon":t(n.arcs);break;case"MultiPolygon":n.arcs.forEach(t)}}function t(n){for(var r=0,t=n.length;r<t;++r,++a)for(var e=n[r],i=0,c=e.length;i<c;++i){var u=e[i];u<0&&(u=~u);var f=o[u];f>=0&&f!==a?o[u]=-1:o[u]=a}}var e,o={},a=0;for(e in n.objects)r(n.objects[e]);return function(n){for(var r,t=0,e=n.length;t<e;++t)if(r=n[t],o[r<0?~r:r]<0)return!0;return!1}},H=function(n){return n},J=function(n){if(null==(r=n.transform))return H;var r,t,e,o=r.scale[0],a=r.scale[1],i=r.translate[0],c=r.translate[1];return function(n,r){return r||(t=e=0),n[0]=(t+=n[0])*o+i,n[1]=(e+=n[1])*a+c,n}},K=function(n){function r(n){c[0]=n[0],c[1]=n[1],i(c),c[0]<u&&(u=c[0]),c[0]>l&&(l=c[0]),c[1]<f&&(f=c[1]),c[1]>s&&(s=c[1])}function t(n){switch(n.type){case"GeometryCollection":n.geometries.forEach(t);break;case"Point":r(n.coordinates);break;case"MultiPoint":n.coordinates.forEach(r)}}var e=n.bbox;if(!e){var o,a,i=J(n),c=new Array(2),u=1/0,f=u,l=-u,s=-u;n.arcs.forEach(function(n){for(var r=-1,t=n.length;++r<t;)o=n[r],c[0]=o[0],c[1]=o[1],i(c,r),c[0]<u&&(u=c[0]),c[0]>l&&(l=c[0]),c[1]<f&&(f=c[1]),c[1]>s&&(s=c[1])});for(a in n.objects)t(n.objects[a]);e=n.bbox=[u,f,l,s]}return e},Q=function(n,r){for(var t,e=n.length,o=e-r;o<--e;)t=n[o],n[o++]=n[e],n[e]=t},X=function(n,r){return"GeometryCollection"===r.type?{type:"FeatureCollection",features:r.geometries.map(function(r){return s(n,r)})}:s(n,r)},Y=function(n,r){function t(r){var t,e=n.arcs[r<0?~r:r],o=e[0];return n.transform?(t=[0,0],e.forEach(function(n){t[0]+=n[0],t[1]+=n[1]})):t=e[e.length-1],r<0?[t,o]:[o,t]}function e(n,r){for(var t in n){var e=n[t];delete r[e.start],delete e.start,delete e.end,e.forEach(function(n){o[n<0?~n:n]=1}),c.push(e)}}var o={},a={},i={},c=[],u=-1;return r.forEach(function(t,e){var o,a=n.arcs[t<0?~t:t];a.length<3&&!a[1][0]&&!a[1][1]&&(o=r[++u],r[u]=t,r[e]=o)}),r.forEach(function(n){var r,e,o=t(n),c=o[0],u=o[1];if(r=i[c])if(delete i[r.end],r.push(n),r.end=u,e=a[u]){delete a[e.start];var f=e===r?r:r.concat(e);a[f.start=r.start]=i[f.end=e.end]=f}else a[r.start]=i[r.end]=r;else if(r=a[u])if(delete a[r.start],r.unshift(n),r.start=c,e=i[c]){delete i[e.end];var l=e===r?r:e.concat(r);a[l.start=e.start]=i[l.end=r.end]=l}else a[r.start]=i[r.end]=r;else r=[n],a[r.start=c]=i[r.end=u]=r}),e(i,a),e(a,i),r.forEach(function(n){o[n<0?~n:n]||c.push([n])}),c},Z=function(n){return h(n,g.apply(this,arguments))},$=function(n){return h(n,y.apply(this,arguments))},nn=function(n,r){for(var t=0,e=n.length;t<e;){var o=t+e>>>1;n[o]<r?t=o+1:e=o}return t},rn=function(n){function r(n,r){n.forEach(function(n){n<0&&(n=~n);var t=o[n];t?t.push(r):o[n]=[r]})}function t(n,t){n.forEach(function(n){r(n,t)})}function e(n,r){"GeometryCollection"===n.type?n.geometries.forEach(function(n){e(n,r)}):n.type in i&&i[n.type](n.arcs,r)}var o={},a=n.map(function(){return[]}),i={LineString:r,MultiLineString:t,Polygon:t,MultiPolygon:function(n,r){n.forEach(function(n){t(n,r)})}};n.forEach(e);for(var c in o)for(var u=o[c],f=u.length,l=0;l<f;++l)for(var s=l+1;s<f;++s){var h,g=u[l],v=u[s];(h=a[g])[c=nn(h,v)]!==v&&h.splice(c,0,v),(h=a[v])[c=nn(h,g)]!==g&&h.splice(c,0,g)}return a},tn=function(n,r){function t(n){n[0]=Math.round((n[0]-i)/c),n[1]=Math.round((n[1]-u)/f)}function e(n){switch(n.type){case"GeometryCollection":n.geometries.forEach(e);break;case"Point":t(n.coordinates);break;case"MultiPoint":n.coordinates.forEach(t)}}if(!((r=Math.floor(r))>=2))throw new Error("n must be ≥2");if(n.transform)throw new Error("already quantized");var o,a=K(n),i=a[0],c=(a[2]-i)/(r-1)||1,u=a[1],f=(a[3]-u)/(r-1)||1;n.arcs.forEach(function(n){for(var r,t,e,o=1,a=1,l=n.length,s=n[0],h=s[0]=Math.round((s[0]-i)/c),g=s[1]=Math.round((s[1]-u)/f);o<l;++o)s=n[o],t=Math.round((s[0]-i)/c),e=Math.round((s[1]-u)/f),t===h&&e===g||(r=n[a++],r[0]=t-h,h=t,r[1]=e-g,g=e);a<2&&(r=n[a++],r[0]=0,r[1]=0),n.length=a});for(o in n.objects)e(n.objects[o]);return n.transform={scale:[c,f],translate:[i,u]},n},en=function(n){if(null==(r=n.transform))return H;var r,t,e,o=r.scale[0],a=r.scale[1],i=r.translate[0],c=r.translate[1];return function(n,r){r||(t=e=0);var u=Math.round((n[0]-i)/o),f=Math.round((n[1]-c)/a);return n[0]=u-t,t=u,n[1]=f-e,e=f,n}},on=function(n,r,t){return r=null==r?Number.MIN_VALUE:+r,null==t&&(t=m),function(e,o){return t(X(n,{type:"Polygon",arcs:[e]}).geometry.coordinates[0],o)>=r}},an=function(){function n(n,r){for(;r>0;){var t=(r+1>>1)-1,o=e[t];if(b(n,o)>=0)break;e[o._=r]=o,e[n._=r=t]=n}}function r(n,r){for(;;){var t=r+1<<1,a=t-1,i=r,c=e[i];if(a<o&&b(e[a],c)<0&&(c=e[i=a]),t<o&&b(e[t],c)<0&&(c=e[i=t]),i===r)break;e[c._=r]=c,e[n._=r=i]=n}}var t={},e=[],o=0;return t.push=function(r){return n(e[r._=o]=r,o++),o},t.pop=function(){if(!(o<=0)){var n,t=e[0];return--o>0&&(n=e[o],r(e[n._=0]=n,0)),t}},t.remove=function(t){var a,i=t._;if(e[i]===t)return i!==--o&&(a=e[o],(b(a,t)<0?n:r)(e[a._=i]=a,i)),i},t},cn=function(n,r){function t(n){a.remove(n),n[1][2]=r(n),a.push(n)}var e=J(n),o=en(n),a=an();return null==r&&(r=d),n.arcs.forEach(function(n){var i,c,u,f=[],l=0;for(n.forEach(e),c=1,u=n.length-1;c<u;++c)i=n.slice(c-1,c+2),i[1][2]=r(i),f.push(i),a.push(i);for(n[0][2]=n[u][2]=1/0,c=0,u=f.length;c<u;++c)i=f[c],i.previous=f[c-1],i.next=f[c+1];for(;i=a.pop();){var s=i.previous,h=i.next;i[1][2]<l?i[1][2]=l:l=i[1][2],s&&(s.next=h,s[2]=i[2],t(s)),h&&(h.previous=s,h[0]=i[0],t(h))}n.forEach(o)}),n},un=function(n,r){var t=[];return n.arcs.forEach(function(n){n.forEach(function(n){isFinite(n[2])&&t.push(n[2])})}),t.length&&E(t.sort(M),r)},fn=function(n,r){return r=null==r?Number.MIN_VALUE:+r,n.arcs.forEach(n.transform?function(n){for(var t,e,o=0,a=0,i=-1,c=-1,u=n.length;++i<u;)t=n[i],t[2]>=r?(e=n[++c],e[0]=t[0]+o,e[1]=t[1]+a,o=a=0):(o+=t[0],a+=t[1]);n.length=++c}:function(n){for(var t,e=-1,o=-1,a=n.length;++e<a;)t=n[e],t[2]>=r&&(n[++o]=t);n.length=++o}),n.arcs.forEach(n.transform?function(n){var r=0,t=0,e=n.length,o=n[0];for(o.length=2;++r<e;)o=n[r],o.length=2,(o[0]||o[1])&&(n[++t]=o);n.length=(t||1)+1}:function(n){var r,t,e=0,o=0,a=n.length,i=n[0],c=i[0],u=i[1];for(i.length=2;++e<a;)i=n[e],r=i[0],t=i[1],i.length=2,c===r&&u===t||(n[++o]=i,c=r,u=t);n.length=(o||1)+1}),n},ln=Math.PI,sn=2*ln,hn=4*ln,gn=ln/180,vn=Math.abs,pn=Math.atan,yn=Math.atan2,dn=Math.cos,mn=Math.max,bn=Math.sin,En=Math.sqrt,Mn=Math.tan;n.topology=V,n.filter=W,n.filterAttached=D,n.filterWeight=on,n.planarRingArea=m,n.planarTriangleArea=d,n.presimplify=cn,n.quantile=un,n.simplify=fn,n.sphericalRingArea=P,n.sphericalTriangleArea=w,n.bbox=K,n.feature=X,n.merge=$,n.mergeArcs=y,n.mesh=Z,n.meshArcs=g,n.neighbors=rn,n.quantize=tn,n.transform=J,n.untransform=en,Object.defineProperty(n,"__esModule",{value:!0})});

 
(function () {
'use strict';

//v1.0 developed for congressional district poverty

//All data for each build/view must be tightly coupled.
//E.g. if data is fetched asynchronously, you must take measures to ensure that any additional data used at the time of
//callback execution is tightly coupled -- e.g. if you set some data, A, with set_data (synchronously), then you execute
//card.json(B) and the card build function relies on both A and B, there's no guarantee that data A will be relevant to
//data B if the UI is such that the user is able to execute set_data() again or execute card.json() again while they
//are waiting for the first card.json result to return.

//the appropriate data can be bound together in a callback passed to card.json(). until a build queuing/data coupling
//scheme is implemented, this is the safest way to use the Card class

//responsiveness at the card level is currently disabled until further edits to the API are made. specifically, the above
//issues need to be taken into consideration and safeguards need to be implemented to prevent browser resize events calling
//build when no data exists yet (or the wrong data exists, per above).

//to do: 1) consider allowing just one view per card to simplify the data handling
// 2) create data slots for each view that are reset to null each time data are fetched for that view. a build cannot proceed until the data slot is not null
// 3) more robust checking. on json request, 
//		a) set default data to null (other data must be checked to exist by callback),
//		b) generate a new id
//		c) wrap each json callba ck in a function that: 1) caches the data, 2) checks the global id still matches the wrapped id, 3) execute callback if still current.

function Card(container){
	//hold references to the dom, specific to this card
	this.dom = {};
	this.container = this.dom.container = container;

	//generic data store for helper data
	this.store = {};

	//default card data
	this.default_data = null;

	//for xhr data requests (e.g. d3.json), cache data by default to avoid unnecessary network requests
	this.data_cache_on = true;
	this.data_cache = [];

	//hold functions used to build the data card views (e.g. high- medium- low-detail)
	this.build_fns = [];
	
	//current view -- could allow a card to have multiple view "levels"
	this.build_index = 0;
	
	//number of registered views
	this.build_count = 0;

	this.responsive_on = false;

	//awaiting data?
	this.fetching_data = false;

	this.print_errors = false;
}

Card.prototype.printErrors = function(a){
	this.print_errors = arguments.length > 0 ? !!a : true;
	return this;
};

//register card building functions
Card.prototype.build = function(fn, view_name){
	if(arguments.length > 0 && typeof fn === "function"){
		var fn_name = !!view_name ? view_name : "View " + (++this.build_count);
		this.build_fns.push({fn:fn, name: fn_name});
	}
	else if(arguments.length == 0 &&
					this.build_index < this.build_fns.length &&
				  this.default_data !== null){
		try{
			//with test of default data !== null, this is not really necessary
			if(this.fetching_data){
				throw "Data not loaded";
			}
			else{
				this.build_fns[this.build_index].fn.call(this, this.default_data);
			}
		}
		catch(e){
			this.error(e);
		}
	}
	else{
		//no-op
	}

	return this;
};

Card.prototype.set_view = function(view){
	//no-op for now

	return this;
};

//get data
Card.prototype.get_data = function(prop){
	if(arguments.length==0){
		return this.default_data;
	}
	else if(arguments.length > 0){
		return this.store.hasOwnProperty(prop) ? this.store[prop] : null;
	}
	else{
		return null;
	}
};

//set data
Card.prototype.set_data = function(data, prop){
	if(arguments.length==1){
		this.default_data = data;
	}
	else if(arguments.length > 1){
		this.store[prop] = data;
	}

	return this;
};

//to do: create a build queue that associates each call to build with data availability
Card.prototype.json = function(uri, callback, prop){
	var self = this;
	var default_data = !prop;

	if(default_data){
		//reset default data if it is being retrieved
		this.default_data = null;
	}

	//check if data has been cached -- if so, use it
	var cached_data = this.check_cache(uri);
	if(cached_data !== null){
		if(default_data){
			this.set_data(cached_data);
		}
		else{
			this.set_data(cached_data, prop);
		}

		if(!callback){this.build();}
		else{callback.call(this, cached_data);}
	}
	else{
		self.fetching_data = true;
		d3.json(uri, function(error, data){
			if(error){
				data = null;
			}
			else if(self.data_cache_on){
				self.data_cache.push({uri:uri, data:data});
			}

			if(default_data){
				self.set_data(data);
			}
			else{
				self.set_data(data, prop);
			}

			//reset before build is called, otherwise build will throw error
			self.fetching_data = false;

			if(!callback){self.build();}
			else{callback.call(self, data);}


		});
	}

	return this;
};

Card.prototype.cache = function(){
	this.data_cache_on = !this.data_cache_on;
	return this;
};

Card.prototype.check_cache = function(uri){
	if(this.data_cache_on && this.data_cache.length > 0){
		var datindex = 0;
		for(var i=0; i<this.data_cache.length; i++){
			if(this.data_cache[i].uri === uri){
				datindex = i;
				break;
			}
		}
		return this.data_cache[datindex].uri === uri ? this.data_cache[datindex].data : null;
	}
	else{
		return null;
	}
};

Card.prototype.error = function(e){
	var wrap = d3.select(this.container);
	wrap.selectAll("*").remove();
	wrap.append("p").style("padding","20px").append("em").html('An error has occured.<br />Please reload the page.<br /><span style="font-size:0.75em;color:#dddddd">'+e+'</span>');
	/*if(this.print_errors){
		wrap.append("p").style("padding","20px").text(e);
	}*/
	return this;
};

Card.prototype.responsive = function(){
	//for now, no-op until there is greater robustness in data handling/coupling of data to build events
	return null;

	if(!this.responsive_on){
		this.responsive_on = true;
		var tiptimer;
		var self = this;
		window.addEventListener("resize", function(){
			clearTimeout(tiptimer);
			tiptimer = setTimeout(function(){
				self.build();
			}, 150);
		});
	}
	return this;
};

//wrapper for the Card class to avoid having to use the new keyword
function card(container){
	var c = new Card(container);
	return c;
}

//v1.0 developed for congressional district poverty and gig economy

function nameshort(longname, appendStateNames){
	try{
		//pull off state names
		var statesplit = longname.split(/,/);
		var states = statesplit[statesplit.length-1];

		var split0 = statesplit[0].split(/--/);
		if(split0.length > 1){
			var shortname = split0[0];
		}
		else{
			var split1 = statesplit[0].split(/-/);
			var shortname = split1[0];
		}
	}
	catch(e){
		var shortname = longname;
	}
	finally{
		var us_exceptions = {"United States":1, "U.S.":1, "US":1, "USA":1, "U.S.A":1};

		if(!!appendStateNames && !(shortname in us_exceptions) ){
			return shortname + ", " + states;
		}
		else{
			return shortname;
		}
		
	}

}

//directory

var dir = {
	rackspace: 'https://c24215cec6c97b637db6-9c0895f07c3474f6636f95b6bf3db172.ssl.cf1.rackcdn.com/interactives/2017',
	ts: (new RegExp("^/|/$", "g")),
	ds: (new RegExp("/{2,}", "g")),
	folders: {}
};

dir.local = function(root_directory){
	if(arguments.length > 0){
		dir.rackspace = root_directory.replace(this.ts, "");
	}
	else{
		dir.rackspace = ".";
	}
	return this;
};

dir.add = function(name, relative_path){
	if(arguments.length==1){
		dir.folders[name] = name.replace(this.ts, "");
	}
	else if(arguments.length > 1){
		dir.folders[name] = relative_path.replace(this.ts, "");
	}
	return this;
};

dir.url = function(name, file_name){
	if(arguments.length==0){
		var path = dir.rackspace;
	}
	else if(arguments.length==1){
		var path = dir.rackspace + "/" + name;
	}
	else{
		var path = dir.rackspace + "/" + dir.folders[name] + "/" + file_name;
	}

	return path.replace(this.ds, "/");
};

//v1.0 developed for congressional district poverty

//viewport dimensions
function dimensions(el, maxwidth, maxheight){
	if(arguments.length > 0){
		var element = el;
	}
	else{
		var element = document.documentElement;
	}

	var floor = 50;
	var err = false;

	try{
		var box = element.getBoundingClientRect();
		var w = Math.floor(box.right - box.left);
		var h = Math.floor(box.bottom - box.top);
		if(w < floor || h < floor){throw "badWidth"}
	}
	catch(e){
		var box = {};
		var w = floor;
		var h = floor;
		err = true;
	}

	if(!!maxwidth && w > maxwidth){w = maxwidth;}
	if(!!maxheight && h > maxheight){h = maxheight;}

	var dim = {width:w, height:h, error:err, box:box};

	return dim;
}

//v1.0 developed for congressional district poverty

//the table function takes a container and an array of associative array objects.



//To do: implement table resizing

/*	T.resize = function(){
		setTimeout(function(){
			try{
				var box = wrap.node();
				
				var thead_box = thead_wrap.node().getBoundingClientRect();
				
				//row 1 of second table
				var r1 = tbr.node();
				var r1box = r1.getBoundingClientRect();
				var r1width = Math.round(r1box.right - r1box.left);

				thead_wrap.style("width", r1width + "px");

					//set heights, eventually
					var boxh = Math.round(box.bottom - box.top);
					var h = boxh - (t1box.bottom-box.top);
					var w = box.right - box.left;
					if(w > 900){
						var fs = "1em";
					}
					else if(w > 500){
						var fs = "0.8em";
					}
					else{
						var fs = "0.65em";
					}

					table_cells.style("font-size", fs);

					if(h < 250){throw "badH"}
					
					two_table_sections.style("height",function(d,i){return i==1 ? h+"px" : "auto"});
			
			}
			catch(e){
				//no-op
			}

		},0);
	}
*/

function metro_select(){
	var sel = {};
	sel.metros = {"large":[{"CBSA_Code":"10420","CBSA_Title":"Akron, OH","POP2010":703200,"lon":-81.3497,"lat":41.1482},{"CBSA_Code":"10580","CBSA_Title":"Albany-Schenectady-Troy, NY","POP2010":870716,"lon":-73.9377,"lat":42.7891},{"CBSA_Code":"10740","CBSA_Title":"Albuquerque, NM","POP2010":887077,"lon":-106.4708,"lat":35.1212},{"CBSA_Code":"10900","CBSA_Title":"Allentown-Bethlehem-Easton, PA-NJ","POP2010":821173,"lon":-75.4018,"lat":40.7883},{"CBSA_Code":"12060","CBSA_Title":"Atlanta-Sandy Springs-Roswell, GA","POP2010":5286728,"lon":-84.3966,"lat":33.6959},{"CBSA_Code":"12260","CBSA_Title":"Augusta-Richmond County, GA-SC","POP2010":564873,"lon":-81.9804,"lat":33.4571},{"CBSA_Code":"12420","CBSA_Title":"Austin-Round Rock, TX","POP2010":1716289,"lon":-97.655,"lat":30.2628},{"CBSA_Code":"12540","CBSA_Title":"Bakersfield, CA","POP2010":839631,"lon":-118.7278,"lat":35.3433},{"CBSA_Code":"12580","CBSA_Title":"Baltimore-Columbia-Towson, MD","POP2010":2710489,"lon":-76.6722,"lat":39.3838},{"CBSA_Code":"12940","CBSA_Title":"Baton Rouge, LA","POP2010":802484,"lon":-91.1324,"lat":30.5709},{"CBSA_Code":"13820","CBSA_Title":"Birmingham-Hoover, AL","POP2010":1128047,"lon":-86.8144,"lat":33.464},{"CBSA_Code":"14260","CBSA_Title":"Boise City, ID","POP2010":616561,"lon":-116.1417,"lat":43.0153},{"CBSA_Code":"14460","CBSA_Title":"Boston-Cambridge-Newton, MA-NH","POP2010":4552402,"lon":-71.1034,"lat":42.5538},{"CBSA_Code":"14860","CBSA_Title":"Bridgeport-Stamford-Norwalk, CT","POP2010":916829,"lon":-73.3891,"lat":41.2683},{"CBSA_Code":"15380","CBSA_Title":"Buffalo-Cheektowaga-Niagara Falls, NY","POP2010":1135509,"lon":-78.7384,"lat":42.9121},{"CBSA_Code":"15980","CBSA_Title":"Cape Coral-Fort Myers, FL","POP2010":618754,"lon":-81.8207,"lat":26.5787},{"CBSA_Code":"16700","CBSA_Title":"Charleston-North Charleston, SC","POP2010":664607,"lon":-80.0441,"lat":33.0416},{"CBSA_Code":"16740","CBSA_Title":"Charlotte-Concord-Gastonia, NC-SC","POP2010":2217012,"lon":-80.8689,"lat":35.1871},{"CBSA_Code":"16860","CBSA_Title":"Chattanooga, TN-GA","POP2010":528143,"lon":-85.3589,"lat":35.0505},{"CBSA_Code":"16980","CBSA_Title":"Chicago-Naperville-Elgin, IL-IN-WI","POP2010":9461105,"lon":-87.964,"lat":41.7035},{"CBSA_Code":"17140","CBSA_Title":"Cincinnati, OH-KY-IN","POP2010":2114580,"lon":-84.4279,"lat":39.0708},{"CBSA_Code":"17460","CBSA_Title":"Cleveland-Elyria, OH","POP2010":2077240,"lon":-81.6839,"lat":41.3755},{"CBSA_Code":"17820","CBSA_Title":"Colorado Springs, CO","POP2010":645613,"lon":-104.6585,"lat":38.8427},{"CBSA_Code":"17900","CBSA_Title":"Columbia, SC","POP2010":767598,"lon":-81.0434,"lat":34.0902},{"CBSA_Code":"18140","CBSA_Title":"Columbus, OH","POP2010":1901974,"lon":-82.8385,"lat":39.9669},{"CBSA_Code":"19100","CBSA_Title":"Dallas-Fort Worth-Arlington, TX","POP2010":6426214,"lon":-97.0252,"lat":32.8182},{"CBSA_Code":"19380","CBSA_Title":"Dayton, OH","POP2010":799232,"lon":-84.14,"lat":39.8295},{"CBSA_Code":"19660","CBSA_Title":"Deltona-Daytona Beach-Ormond Beach, FL","POP2010":590289,"lon":-81.2182,"lat":29.1699},{"CBSA_Code":"19740","CBSA_Title":"Denver-Aurora-Lakewood, CO","POP2010":2543482,"lon":-104.8942,"lat":39.4342},{"CBSA_Code":"19780","CBSA_Title":"Des Moines-West Des Moines, IA","POP2010":569633,"lon":-93.9431,"lat":41.5479},{"CBSA_Code":"19820","CBSA_Title":"Detroit-Warren-Dearborn, MI","POP2010":4296250,"lon":-83.2333,"lat":42.7203},{"CBSA_Code":"21340","CBSA_Title":"El Paso, TX","POP2010":804123,"lon":-105.5386,"lat":31.5118},{"CBSA_Code":"23420","CBSA_Title":"Fresno, CA","POP2010":930450,"lon":-119.6492,"lat":36.7566},{"CBSA_Code":"24340","CBSA_Title":"Grand Rapids-Wyoming, MI","POP2010":988938,"lon":-85.4883,"lat":42.9988},{"CBSA_Code":"24660","CBSA_Title":"Greensboro-High Point, NC","POP2010":723801,"lon":-79.7913,"lat":36.0264},{"CBSA_Code":"24860","CBSA_Title":"Greenville-Anderson-Mauldin, SC","POP2010":824112,"lon":-82.4168,"lat":34.6889},{"CBSA_Code":"25420","CBSA_Title":"Harrisburg-Carlisle, PA","POP2010":549475,"lon":-77.0945,"lat":40.3278},{"CBSA_Code":"25540","CBSA_Title":"Hartford-West Hartford-East Hartford, CT","POP2010":1212381,"lon":-72.5789,"lat":41.7326},{"CBSA_Code":"26420","CBSA_Title":"Houston-The Woodlands-Sugar Land, TX","POP2010":5920416,"lon":-95.3965,"lat":29.7819},{"CBSA_Code":"26900","CBSA_Title":"Indianapolis-Carmel-Anderson, IN","POP2010":1887877,"lon":-86.2069,"lat":39.7468},{"CBSA_Code":"27140","CBSA_Title":"Jackson, MS","POP2010":567122,"lon":-90.2216,"lat":32.3171},{"CBSA_Code":"27260","CBSA_Title":"Jacksonville, FL","POP2010":1345596,"lon":-81.7926,"lat":30.2365},{"CBSA_Code":"28140","CBSA_Title":"Kansas City, MO-KS","POP2010":2009342,"lon":-94.4464,"lat":38.9368},{"CBSA_Code":"28940","CBSA_Title":"Knoxville, TN","POP2010":837571,"lon":-84.1358,"lat":36.0434},{"CBSA_Code":"29460","CBSA_Title":"Lakeland-Winter Haven, FL","POP2010":602095,"lon":-81.6991,"lat":27.9503},{"CBSA_Code":"29820","CBSA_Title":"Las Vegas-Henderson-Paradise, NV","POP2010":1951269,"lon":-115.0156,"lat":36.2149},{"CBSA_Code":"30780","CBSA_Title":"Little Rock-North Little Rock-Conway, AR","POP2010":699757,"lon":-92.396,"lat":34.7559},{"CBSA_Code":"31080","CBSA_Title":"Los Angeles-Long Beach-Anaheim, CA","POP2010":12828837,"lon":-118.1388,"lat":34.2474},{"CBSA_Code":"31140","CBSA_Title":"Louisville/Jefferson County, KY-IN","POP2010":1235708,"lon":-85.67,"lat":38.3371},{"CBSA_Code":"31540","CBSA_Title":"Madison, WI","POP2010":605435,"lon":-89.591,"lat":43.0794},{"CBSA_Code":"32580","CBSA_Title":"McAllen-Edinburg-Mission, TX","POP2010":774769,"lon":-98.1806,"lat":26.3964},{"CBSA_Code":"32820","CBSA_Title":"Memphis, TN-MS-AR","POP2010":1324829,"lon":-89.8152,"lat":35.0076},{"CBSA_Code":"33100","CBSA_Title":"Miami-Fort Lauderdale-West Palm Beach, FL","POP2010":5564635,"lon":-80.5059,"lat":26.1607},{"CBSA_Code":"33340","CBSA_Title":"Milwaukee-Waukesha-West Allis, WI","POP2010":1555908,"lon":-88.1734,"lat":43.1773},{"CBSA_Code":"33460","CBSA_Title":"Minneapolis-St. Paul-Bloomington, MN-WI","POP2010":3348859,"lon":-93.3463,"lat":45.0657},{"CBSA_Code":"34980","CBSA_Title":"Nashville-Davidson--Murfreesboro--Franklin, TN","POP2010":1670890,"lon":-86.7249,"lat":36.0881},{"CBSA_Code":"35300","CBSA_Title":"New Haven-Milford, CT","POP2010":862477,"lon":-72.9377,"lat":41.412},{"CBSA_Code":"35380","CBSA_Title":"New Orleans-Metairie, LA","POP2010":1189866,"lon":-89.9602,"lat":29.9184},{"CBSA_Code":"35620","CBSA_Title":"New York-Newark-Jersey City, NY-NJ-PA","POP2010":19567410,"lon":-74.0892,"lat":40.9223},{"CBSA_Code":"35840","CBSA_Title":"North Port-Sarasota-Bradenton, FL","POP2010":702281,"lon":-82.3224,"lat":27.3478},{"CBSA_Code":"36260","CBSA_Title":"Ogden-Clearfield, UT","POP2010":597159,"lon":-112.8181,"lat":41.4327},{"CBSA_Code":"36420","CBSA_Title":"Oklahoma City, OK","POP2010":1252987,"lon":-97.5049,"lat":35.4287},{"CBSA_Code":"36540","CBSA_Title":"Omaha-Council Bluffs, NE-IA","POP2010":865350,"lon":-95.9998,"lat":41.2904},{"CBSA_Code":"36740","CBSA_Title":"Orlando-Kissimmee-Sanford, FL","POP2010":2134411,"lon":-81.3636,"lat":28.4335},{"CBSA_Code":"37100","CBSA_Title":"Oxnard-Thousand Oaks-Ventura, CA","POP2010":823318,"lon":-119.0789,"lat":34.4731},{"CBSA_Code":"37340","CBSA_Title":"Palm Bay-Melbourne-Titusville, FL","POP2010":543376,"lon":-80.7325,"lat":28.2938},{"CBSA_Code":"37980","CBSA_Title":"Philadelphia-Camden-Wilmington, PA-NJ-DE-MD","POP2010":5965343,"lon":-75.3032,"lat":39.9046},{"CBSA_Code":"38060","CBSA_Title":"Phoenix-Mesa-Scottsdale, AZ","POP2010":4192887,"lon":-112.0707,"lat":33.1858},{"CBSA_Code":"38300","CBSA_Title":"Pittsburgh, PA","POP2010":2356285,"lon":-79.8309,"lat":40.4394},{"CBSA_Code":"38900","CBSA_Title":"Portland-Vancouver-Hillsboro, OR-WA","POP2010":2226009,"lon":-122.4783,"lat":45.5976},{"CBSA_Code":"39300","CBSA_Title":"Providence-Warwick, RI-MA","POP2010":1600852,"lon":-71.3998,"lat":41.7242},{"CBSA_Code":"39340","CBSA_Title":"Provo-Orem, UT","POP2010":526810,"lon":-112.3536,"lat":39.8642},{"CBSA_Code":"39580","CBSA_Title":"Raleigh, NC","POP2010":1130490,"lon":-78.4617,"lat":35.7539},{"CBSA_Code":"40060","CBSA_Title":"Richmond, VA","POP2010":1208101,"lon":-77.4725,"lat":37.4604},{"CBSA_Code":"40140","CBSA_Title":"Riverside-San Bernardino-Ontario, CA","POP2010":4224851,"lon":-116.1282,"lat":34.5522},{"CBSA_Code":"40380","CBSA_Title":"Rochester, NY","POP2010":1079671,"lon":-77.5095,"lat":42.9688},{"CBSA_Code":"40900","CBSA_Title":"Sacramento--Roseville--Arden-Arcade, CA","POP2010":2149127,"lon":-120.9985,"lat":38.7812},{"CBSA_Code":"41180","CBSA_Title":"St. Louis, MO-IL","POP2010":2787701,"lon":-90.3499,"lat":38.7336},{"CBSA_Code":"41620","CBSA_Title":"Salt Lake City, UT","POP2010":1087873,"lon":-113.0109,"lat":40.4709},{"CBSA_Code":"41700","CBSA_Title":"San Antonio-New Braunfels, TX","POP2010":2142508,"lon":-98.6015,"lat":29.4283},{"CBSA_Code":"41740","CBSA_Title":"San Diego-Carlsbad, CA","POP2010":3095313,"lon":-116.7319,"lat":33.0335},{"CBSA_Code":"41860","CBSA_Title":"San Francisco-Oakland-Hayward, CA","POP2010":4335391,"lon":-122.0149,"lat":37.7021},{"CBSA_Code":"41940","CBSA_Title":"San Jose-Sunnyvale-Santa Clara, CA","POP2010":1836911,"lon":-121.3745,"lat":36.909},{"CBSA_Code":"42540","CBSA_Title":"Scranton--Wilkes-Barre--Hazleton, PA","POP2010":563631,"lon":-75.8945,"lat":41.3231},{"CBSA_Code":"42660","CBSA_Title":"Seattle-Tacoma-Bellevue, WA","POP2010":3439809,"lon":-121.8656,"lat":47.5534},{"CBSA_Code":"44060","CBSA_Title":"Spokane-Spokane Valley, WA","POP2010":527753,"lon":-117.5722,"lat":48.1934},{"CBSA_Code":"44140","CBSA_Title":"Springfield, MA","POP2010":621570,"lon":-72.6448,"lat":42.2292},{"CBSA_Code":"44700","CBSA_Title":"Stockton-Lodi, CA","POP2010":685306,"lon":-121.2723,"lat":37.9323},{"CBSA_Code":"45060","CBSA_Title":"Syracuse, NY","POP2010":662577,"lon":-76.0338,"lat":43.1568},{"CBSA_Code":"45300","CBSA_Title":"Tampa-St. Petersburg-Clearwater, FL","POP2010":2783243,"lon":-82.4056,"lat":28.1543},{"CBSA_Code":"45780","CBSA_Title":"Toledo, OH","POP2010":610001,"lon":-83.7804,"lat":41.4986},{"CBSA_Code":"46060","CBSA_Title":"Tucson, AZ","POP2010":980263,"lon":-111.79,"lat":32.0974},{"CBSA_Code":"46140","CBSA_Title":"Tulsa, OK","POP2010":937478,"lon":-96.1654,"lat":36.2496},{"CBSA_Code":"46520","CBSA_Title":"Urban Honolulu, HI","POP2010":953207,"lon":-157.9757,"lat":21.4604},{"CBSA_Code":"47260","CBSA_Title":"Virginia Beach-Norfolk-Newport News, VA-NC","POP2010":1676822,"lon":-76.4147,"lat":36.6557},{"CBSA_Code":"47900","CBSA_Title":"Washington-Arlington-Alexandria, DC-VA-MD-WV","POP2010":5636232,"lon":-77.4724,"lat":38.8319},{"CBSA_Code":"48620","CBSA_Title":"Wichita, KS","POP2010":630919,"lon":-97.3981,"lat":37.625},{"CBSA_Code":"49180","CBSA_Title":"Winston-Salem, NC","POP2010":640595,"lon":-80.3451,"lat":36.0724},{"CBSA_Code":"49340","CBSA_Title":"Worcester, MA-CT","POP2010":916980,"lon":-71.9287,"lat":42.2188},{"CBSA_Code":"49660","CBSA_Title":"Youngstown-Warren-Boardman, OH-PA","POP2010":565773,"lon":-80.5642,"lat":41.2417}],"small":[{"CBSA_Code":"10180","CBSA_Title":"Abilene, TX","POP2010":165252,"lon":-99.7176,"lat":32.4498},{"CBSA_Code":"10500","CBSA_Title":"Albany, GA","POP2010":157308,"lon":-84.1702,"lat":31.5866},{"CBSA_Code":"10540","CBSA_Title":"Albany, OR","POP2010":116672,"lon":-122.5384,"lat":44.4886},{"CBSA_Code":"10780","CBSA_Title":"Alexandria, LA","POP2010":153922,"lon":-92.5438,"lat":31.3346},{"CBSA_Code":"11020","CBSA_Title":"Altoona, PA","POP2010":127089,"lon":-78.3472,"lat":40.4834},{"CBSA_Code":"11100","CBSA_Title":"Amarillo, TX","POP2010":251933,"lon":-101.9104,"lat":35.2488},{"CBSA_Code":"11180","CBSA_Title":"Ames, IA","POP2010":89542,"lon":-93.465,"lat":42.0362},{"CBSA_Code":"11260","CBSA_Title":"Anchorage, AK","POP2010":380821,"lon":-149.5429,"lat":62.237},{"CBSA_Code":"11460","CBSA_Title":"Ann Arbor, MI","POP2010":344791,"lon":-83.8385,"lat":42.2531},{"CBSA_Code":"11500","CBSA_Title":"Anniston-Oxford-Jacksonville, AL","POP2010":118572,"lon":-85.8243,"lat":33.7738},{"CBSA_Code":"11540","CBSA_Title":"Appleton, WI","POP2010":225666,"lon":-88.3714,"lat":44.2887},{"CBSA_Code":"11700","CBSA_Title":"Asheville, NC","POP2010":424858,"lon":-82.6853,"lat":35.6016},{"CBSA_Code":"12020","CBSA_Title":"Athens-Clarke County, GA","POP2010":192541,"lon":-83.2169,"lat":33.9503},{"CBSA_Code":"12100","CBSA_Title":"Atlantic City-Hammonton, NJ","POP2010":274549,"lon":-74.6609,"lat":39.4777},{"CBSA_Code":"12220","CBSA_Title":"Auburn-Opelika, AL","POP2010":140247,"lon":-85.3593,"lat":32.6077},{"CBSA_Code":"12620","CBSA_Title":"Bangor, ME","POP2010":153923,"lon":-68.6504,"lat":45.4015},{"CBSA_Code":"12700","CBSA_Title":"Barnstable Town, MA","POP2010":215888,"lon":-70.2921,"lat":41.7237},{"CBSA_Code":"12980","CBSA_Title":"Battle Creek, MI","POP2010":136146,"lon":-85.0049,"lat":42.2463},{"CBSA_Code":"13020","CBSA_Title":"Bay City, MI","POP2010":107771,"lon":-83.9916,"lat":43.7014},{"CBSA_Code":"13140","CBSA_Title":"Beaumont-Port Arthur, TX","POP2010":403190,"lon":-94.0707,"lat":30.305},{"CBSA_Code":"13220","CBSA_Title":"Beckley, WV","POP2010":124898,"lon":-81.1611,"lat":37.9103},{"CBSA_Code":"13380","CBSA_Title":"Bellingham, WA","POP2010":201140,"lon":-121.7125,"lat":48.8258},{"CBSA_Code":"13460","CBSA_Title":"Bend-Redmond, OR","POP2010":157733,"lon":-121.2277,"lat":43.9145},{"CBSA_Code":"13740","CBSA_Title":"Billings, MT","POP2010":158934,"lon":-108.7152,"lat":45.7811},{"CBSA_Code":"13780","CBSA_Title":"Binghamton, NY","POP2010":251725,"lon":-76.0265,"lat":42.1623},{"CBSA_Code":"13900","CBSA_Title":"Bismarck, ND","POP2010":114778,"lon":-100.9905,"lat":46.7286},{"CBSA_Code":"13980","CBSA_Title":"Blacksburg-Christiansburg-Radford, VA","POP2010":178237,"lon":-80.5332,"lat":37.1201},{"CBSA_Code":"14010","CBSA_Title":"Bloomington, IL","POP2010":186133,"lon":-88.8634,"lat":40.4122},{"CBSA_Code":"14020","CBSA_Title":"Bloomington, IN","POP2010":159549,"lon":-86.6768,"lat":39.2341},{"CBSA_Code":"14100","CBSA_Title":"Bloomsburg-Berwick, PA","POP2010":85562,"lon":-76.457,"lat":41.0392},{"CBSA_Code":"14500","CBSA_Title":"Boulder, CO","POP2010":294567,"lon":-105.3586,"lat":40.0934},{"CBSA_Code":"14540","CBSA_Title":"Bowling Green, KY","POP2010":158599,"lon":-86.4092,"lat":37.0426},{"CBSA_Code":"14740","CBSA_Title":"Bremerton-Silverdale, WA","POP2010":251133,"lon":-122.6769,"lat":47.6121},{"CBSA_Code":"15180","CBSA_Title":"Brownsville-Harlingen, TX","POP2010":406220,"lon":-97.5333,"lat":26.128},{"CBSA_Code":"15260","CBSA_Title":"Brunswick, GA","POP2010":112370,"lon":-81.6329,"lat":31.3194},{"CBSA_Code":"15500","CBSA_Title":"Burlington, NC","POP2010":151131,"lon":-79.3989,"lat":36.0436},{"CBSA_Code":"15540","CBSA_Title":"Burlington-South Burlington, VT","POP2010":211261,"lon":-73.0301,"lat":44.6905},{"CBSA_Code":"15680","CBSA_Title":"California-Lexington Park, MD","POP2010":105151,"lon":-76.609,"lat":38.3027},{"CBSA_Code":"15940","CBSA_Title":"Canton-Massillon, OH","POP2010":404422,"lon":-81.2498,"lat":40.7179},{"CBSA_Code":"16020","CBSA_Title":"Cape Girardeau, MO-IL","POP2010":96275,"lon":-89.7715,"lat":37.3235},{"CBSA_Code":"16060","CBSA_Title":"Carbondale-Marion, IL","POP2010":126575,"lon":-89.1905,"lat":37.7622},{"CBSA_Code":"16180","CBSA_Title":"Carson City, NV","POP2010":55274,"lon":-119.7423,"lat":39.1584},{"CBSA_Code":"16220","CBSA_Title":"Casper, WY","POP2010":75450,"lon":-106.798,"lat":42.9624},{"CBSA_Code":"16300","CBSA_Title":"Cedar Rapids, IA","POP2010":257940,"lon":-91.6314,"lat":42.0915},{"CBSA_Code":"16540","CBSA_Title":"Chambersburg-Waynesboro, PA","POP2010":149618,"lon":-77.7185,"lat":39.9289},{"CBSA_Code":"16580","CBSA_Title":"Champaign-Urbana, IL","POP2010":231891,"lon":-88.2963,"lat":40.226},{"CBSA_Code":"16620","CBSA_Title":"Charleston, WV","POP2010":227078,"lon":-81.495,"lat":38.2708},{"CBSA_Code":"16820","CBSA_Title":"Charlottesville, VA","POP2010":218705,"lon":-78.5765,"lat":37.8507},{"CBSA_Code":"16940","CBSA_Title":"Cheyenne, WY","POP2010":91738,"lon":-104.6888,"lat":41.3071},{"CBSA_Code":"17020","CBSA_Title":"Chico, CA","POP2010":220000,"lon":-121.5987,"lat":39.6692},{"CBSA_Code":"17300","CBSA_Title":"Clarksville, TN-KY","POP2010":260625,"lon":-87.5642,"lat":36.7475},{"CBSA_Code":"17420","CBSA_Title":"Cleveland, TN","POP2010":115788,"lon":-84.6643,"lat":35.1362},{"CBSA_Code":"17660","CBSA_Title":"Coeur d'Alene, ID","POP2010":138494,"lon":-116.7006,"lat":47.6732},{"CBSA_Code":"17780","CBSA_Title":"College Station-Bryan, TX","POP2010":228660,"lon":-96.491,"lat":30.7573},{"CBSA_Code":"17860","CBSA_Title":"Columbia, MO","POP2010":162642,"lon":-92.3056,"lat":38.9881},{"CBSA_Code":"17980","CBSA_Title":"Columbus, GA-AL","POP2010":294865,"lon":-84.9134,"lat":32.4419},{"CBSA_Code":"18020","CBSA_Title":"Columbus, IN","POP2010":76794,"lon":-85.8967,"lat":39.2091},{"CBSA_Code":"18580","CBSA_Title":"Corpus Christi, TX","POP2010":428185,"lon":-97.4954,"lat":27.9026},{"CBSA_Code":"18700","CBSA_Title":"Corvallis, OR","POP2010":85579,"lon":-123.4291,"lat":44.4911},{"CBSA_Code":"18880","CBSA_Title":"Crestview-Fort Walton Beach-Destin, FL","POP2010":235865,"lon":-86.3655,"lat":30.6655},{"CBSA_Code":"19060","CBSA_Title":"Cumberland, MD-WV","POP2010":103299,"lon":-78.8059,"lat":39.5294},{"CBSA_Code":"19140","CBSA_Title":"Dalton, GA","POP2010":142227,"lon":-84.8458,"lat":34.8014},{"CBSA_Code":"19180","CBSA_Title":"Danville, IL","POP2010":81625,"lon":-87.732,"lat":40.1818},{"CBSA_Code":"19300","CBSA_Title":"Daphne-Fairhope-Foley, AL","POP2010":182265,"lon":-87.7227,"lat":30.7294},{"CBSA_Code":"19340","CBSA_Title":"Davenport-Moline-Rock Island, IA-IL","POP2010":379690,"lon":-90.4685,"lat":41.3966},{"CBSA_Code":"19460","CBSA_Title":"Decatur, AL","POP2010":153829,"lon":-87.1009,"lat":34.4885},{"CBSA_Code":"19500","CBSA_Title":"Decatur, IL","POP2010":110768,"lon":-88.9634,"lat":39.8606},{"CBSA_Code":"20020","CBSA_Title":"Dothan, AL","POP2010":145639,"lon":-85.4551,"lat":31.2583},{"CBSA_Code":"20100","CBSA_Title":"Dover, DE","POP2010":162310,"lon":-75.5683,"lat":39.0857},{"CBSA_Code":"20220","CBSA_Title":"Dubuque, IA","POP2010":93653,"lon":-90.8809,"lat":42.4687},{"CBSA_Code":"20260","CBSA_Title":"Duluth, MN-WI","POP2010":279771,"lon":-92.407,"lat":47.3334},{"CBSA_Code":"20500","CBSA_Title":"Durham-Chapel Hill, NC","POP2010":504357,"lon":-79.1005,"lat":35.994},{"CBSA_Code":"20700","CBSA_Title":"East Stroudsburg, PA","POP2010":169842,"lon":-75.3433,"lat":41.0587},{"CBSA_Code":"20740","CBSA_Title":"Eau Claire, WI","POP2010":161151,"lon":-91.2827,"lat":44.9392},{"CBSA_Code":"20940","CBSA_Title":"El Centro, CA","POP2010":174528,"lon":-115.3627,"lat":33.0405},{"CBSA_Code":"21060","CBSA_Title":"Elizabethtown-Fort Knox, KY","POP2010":148338,"lon":-85.9718,"lat":37.7352},{"CBSA_Code":"21140","CBSA_Title":"Elkhart-Goshen, IN","POP2010":197559,"lon":-85.8588,"lat":41.5977},{"CBSA_Code":"21300","CBSA_Title":"Elmira, NY","POP2010":88830,"lon":-76.7638,"lat":42.1393},{"CBSA_Code":"21500","CBSA_Title":"Erie, PA","POP2010":280566,"lon":-80.033,"lat":41.9925},{"CBSA_Code":"21660","CBSA_Title":"Eugene, OR","POP2010":351715,"lon":-122.8454,"lat":43.938},{"CBSA_Code":"21780","CBSA_Title":"Evansville, IN-KY","POP2010":311552,"lon":-87.5767,"lat":37.9678},{"CBSA_Code":"21820","CBSA_Title":"Fairbanks, AK","POP2010":97581,"lon":-146.5673,"lat":64.8071},{"CBSA_Code":"22020","CBSA_Title":"Fargo, ND-MN","POP2010":208777,"lon":-96.9612,"lat":46.9175},{"CBSA_Code":"22140","CBSA_Title":"Farmington, NM","POP2010":130044,"lon":-108.3204,"lat":36.5082},{"CBSA_Code":"22180","CBSA_Title":"Fayetteville, NC","POP2010":366383,"lon":-78.9786,"lat":35.0347},{"CBSA_Code":"22220","CBSA_Title":"Fayetteville-Springdale-Rogers, AR-MO","POP2010":463204,"lon":-94.1206,"lat":36.1954},{"CBSA_Code":"22380","CBSA_Title":"Flagstaff, AZ","POP2010":134421,"lon":-111.7711,"lat":35.8392},{"CBSA_Code":"22420","CBSA_Title":"Flint, MI","POP2010":425790,"lon":-83.7059,"lat":43.0224},{"CBSA_Code":"22500","CBSA_Title":"Florence, SC","POP2010":205566,"lon":-79.8073,"lat":34.1537},{"CBSA_Code":"22520","CBSA_Title":"Florence-Muscle Shoals, AL","POP2010":147137,"lon":-87.7207,"lat":34.8082},{"CBSA_Code":"22540","CBSA_Title":"Fond du Lac, WI","POP2010":101633,"lon":-88.4889,"lat":43.7535},{"CBSA_Code":"22660","CBSA_Title":"Fort Collins, CO","POP2010":299630,"lon":-105.4614,"lat":40.6664},{"CBSA_Code":"22900","CBSA_Title":"Fort Smith, AR-OK","POP2010":280467,"lon":-94.5662,"lat":35.1873},{"CBSA_Code":"23060","CBSA_Title":"Fort Wayne, IN","POP2010":416257,"lon":-85.2165,"lat":41.0055},{"CBSA_Code":"23460","CBSA_Title":"Gadsden, AL","POP2010":104430,"lon":-86.0365,"lat":34.0425},{"CBSA_Code":"23540","CBSA_Title":"Gainesville, FL","POP2010":264275,"lon":-82.4723,"lat":29.6891},{"CBSA_Code":"23580","CBSA_Title":"Gainesville, GA","POP2010":179684,"lon":-83.8197,"lat":34.3217},{"CBSA_Code":"23900","CBSA_Title":"Gettysburg, PA","POP2010":101407,"lon":-77.2204,"lat":39.8677},{"CBSA_Code":"24020","CBSA_Title":"Glens Falls, NY","POP2010":128923,"lon":-73.6492,"lat":43.4454},{"CBSA_Code":"24140","CBSA_Title":"Goldsboro, NC","POP2010":122623,"lon":-77.9985,"lat":35.3583},{"CBSA_Code":"24220","CBSA_Title":"Grand Forks, ND-MN","POP2010":98461,"lon":-96.8452,"lat":47.8362},{"CBSA_Code":"24260","CBSA_Title":"Grand Island, NE","POP2010":81850,"lon":-98.2761,"lat":41.0339},{"CBSA_Code":"24300","CBSA_Title":"Grand Junction, CO","POP2010":146723,"lon":-108.4687,"lat":39.0175},{"CBSA_Code":"24420","CBSA_Title":"Grants Pass, OR","POP2010":82713,"lon":-123.5563,"lat":42.3642},{"CBSA_Code":"24500","CBSA_Title":"Great Falls, MT","POP2010":81327,"lon":-111.3472,"lat":47.3087},{"CBSA_Code":"24540","CBSA_Title":"Greeley, CO","POP2010":252825,"lon":-104.393,"lat":40.5551},{"CBSA_Code":"24580","CBSA_Title":"Green Bay, WI","POP2010":306241,"lon":-88.0775,"lat":44.7753},{"CBSA_Code":"24780","CBSA_Title":"Greenville, NC","POP2010":168148,"lon":-77.3716,"lat":35.5907},{"CBSA_Code":"25060","CBSA_Title":"Gulfport-Biloxi-Pascagoula, MS","POP2010":370702,"lon":-89.0197,"lat":30.5005},{"CBSA_Code":"25180","CBSA_Title":"Hagerstown-Martinsburg, MD-WV","POP2010":251599,"lon":-77.8997,"lat":39.5465},{"CBSA_Code":"25220","CBSA_Title":"Hammond, LA","POP2010":121097,"lon":-90.4062,"lat":30.629},{"CBSA_Code":"25260","CBSA_Title":"Hanford-Corcoran, CA","POP2010":152982,"lon":-119.8139,"lat":36.0761},{"CBSA_Code":"25500","CBSA_Title":"Harrisonburg, VA","POP2010":125228,"lon":-78.8717,"lat":38.5116},{"CBSA_Code":"25620","CBSA_Title":"Hattiesburg, MS","POP2010":142842,"lon":-89.2307,"lat":31.186},{"CBSA_Code":"25860","CBSA_Title":"Hickory-Lenoir-Morganton, NC","POP2010":365497,"lon":-81.4572,"lat":35.8141},{"CBSA_Code":"25940","CBSA_Title":"Hilton Head Island-Bluffton-Beaufort, SC","POP2010":187010,"lon":-80.8718,"lat":32.4067},{"CBSA_Code":"25980","CBSA_Title":"Hinesville, GA","POP2010":77917,"lon":-81.6045,"lat":31.798},{"CBSA_Code":"26140","CBSA_Title":"Homosassa Springs, FL","POP2010":141236,"lon":-82.4596,"lat":28.8547},{"CBSA_Code":"26300","CBSA_Title":"Hot Springs, AR","POP2010":96024,"lon":-93.1492,"lat":34.5769},{"CBSA_Code":"26380","CBSA_Title":"Houma-Thibodaux, LA","POP2010":208178,"lon":-90.662,"lat":29.4694},{"CBSA_Code":"26580","CBSA_Title":"Huntington-Ashland, WV-KY-OH","POP2010":364908,"lon":-82.3832,"lat":38.3789},{"CBSA_Code":"26620","CBSA_Title":"Huntsville, AL","POP2010":417593,"lon":-86.7352,"lat":34.7831},{"CBSA_Code":"26820","CBSA_Title":"Idaho Falls, ID","POP2010":133265,"lon":-112.4338,"lat":43.6242},{"CBSA_Code":"26980","CBSA_Title":"Iowa City, IA","POP2010":152586,"lon":-91.6495,"lat":41.5112},{"CBSA_Code":"27060","CBSA_Title":"Ithaca, NY","POP2010":101564,"lon":-76.4705,"lat":42.4487},{"CBSA_Code":"27100","CBSA_Title":"Jackson, MI","POP2010":160248,"lon":-84.4226,"lat":42.2481},{"CBSA_Code":"27180","CBSA_Title":"Jackson, TN","POP2010":130011,"lon":-88.8467,"lat":35.609},{"CBSA_Code":"27340","CBSA_Title":"Jacksonville, NC","POP2010":177772,"lon":-77.4262,"lat":34.7271},{"CBSA_Code":"27500","CBSA_Title":"Janesville-Beloit, WI","POP2010":160331,"lon":-89.0715,"lat":42.6713},{"CBSA_Code":"27620","CBSA_Title":"Jefferson City, MO","POP2010":149807,"lon":-92.0909,"lat":38.6398},{"CBSA_Code":"27740","CBSA_Title":"Johnson City, TN","POP2010":198716,"lon":-82.3373,"lat":36.2518},{"CBSA_Code":"27780","CBSA_Title":"Johnstown, PA","POP2010":143679,"lon":-78.7205,"lat":40.4911},{"CBSA_Code":"27860","CBSA_Title":"Jonesboro, AR","POP2010":121026,"lon":-90.648,"lat":35.6986},{"CBSA_Code":"27900","CBSA_Title":"Joplin, MO","POP2010":175518,"lon":-94.34,"lat":37.0568},{"CBSA_Code":"27980","CBSA_Title":"Kahului-Wailuku-Lahaina, HI","POP2010":154924,"lon":-156.3397,"lat":20.7937},{"CBSA_Code":"28020","CBSA_Title":"Kalamazoo-Portage, MI","POP2010":326589,"lon":-85.7836,"lat":42.2486},{"CBSA_Code":"28100","CBSA_Title":"Kankakee, IL","POP2010":113449,"lon":-87.8621,"lat":41.1375},{"CBSA_Code":"28420","CBSA_Title":"Kennewick-Richland, WA","POP2010":253340,"lon":-119.2565,"lat":46.3638},{"CBSA_Code":"28660","CBSA_Title":"Killeen-Temple, TX","POP2010":405300,"lon":-97.7876,"lat":31.2079},{"CBSA_Code":"28700","CBSA_Title":"Kingsport-Bristol-Bristol, TN-VA","POP2010":309544,"lon":-82.4389,"lat":36.6113},{"CBSA_Code":"28740","CBSA_Title":"Kingston, NY","POP2010":182493,"lon":-74.2626,"lat":41.8888},{"CBSA_Code":"29020","CBSA_Title":"Kokomo, IN","POP2010":82752,"lon":-86.1135,"lat":40.4853},{"CBSA_Code":"29100","CBSA_Title":"La Crosse-Onalaska, WI-MN","POP2010":133665,"lon":-91.32,"lat":43.7797},{"CBSA_Code":"29180","CBSA_Title":"Lafayette, LA","POP2010":466750,"lon":-92.0602,"lat":30.021},{"CBSA_Code":"29200","CBSA_Title":"Lafayette-West Lafayette, IN","POP2010":201789,"lon":-86.9275,"lat":40.5151},{"CBSA_Code":"29340","CBSA_Title":"Lake Charles, LA","POP2010":199607,"lon":-93.2577,"lat":30.0169},{"CBSA_Code":"29420","CBSA_Title":"Lake Havasu City-Kingman, AZ","POP2010":200186,"lon":-113.7596,"lat":35.7057},{"CBSA_Code":"29540","CBSA_Title":"Lancaster, PA","POP2010":519445,"lon":-76.2445,"lat":40.0401},{"CBSA_Code":"29620","CBSA_Title":"Lansing-East Lansing, MI","POP2010":464036,"lon":-84.6075,"lat":42.7137},{"CBSA_Code":"29700","CBSA_Title":"Laredo, TX","POP2010":250304,"lon":-99.3331,"lat":27.7619},{"CBSA_Code":"29740","CBSA_Title":"Las Cruces, NM","POP2010":209233,"lon":-106.8324,"lat":32.3518},{"CBSA_Code":"29940","CBSA_Title":"Lawrence, KS","POP2010":110826,"lon":-95.2907,"lat":38.8822},{"CBSA_Code":"30020","CBSA_Title":"Lawton, OK","POP2010":130291,"lon":-98.4339,"lat":34.5253},{"CBSA_Code":"30140","CBSA_Title":"Lebanon, PA","POP2010":133568,"lon":-76.4565,"lat":40.3668},{"CBSA_Code":"30300","CBSA_Title":"Lewiston, ID-WA","POP2010":60888,"lon":-116.9405,"lat":46.267},{"CBSA_Code":"30340","CBSA_Title":"Lewiston-Auburn, ME","POP2010":107702,"lon":-70.2029,"lat":44.1664},{"CBSA_Code":"30460","CBSA_Title":"Lexington-Fayette, KY","POP2010":472099,"lon":-84.4314,"lat":38.0925},{"CBSA_Code":"30620","CBSA_Title":"Lima, OH","POP2010":106331,"lon":-84.1124,"lat":40.7696},{"CBSA_Code":"30700","CBSA_Title":"Lincoln, NE","POP2010":302157,"lon":-96.8708,"lat":40.8199},{"CBSA_Code":"30860","CBSA_Title":"Logan, UT-ID","POP2010":125442,"lon":-111.7689,"lat":41.8869},{"CBSA_Code":"30980","CBSA_Title":"Longview, TX","POP2010":214369,"lon":-94.829,"lat":32.3731},{"CBSA_Code":"31020","CBSA_Title":"Longview, WA","POP2010":102410,"lon":-122.6795,"lat":46.193},{"CBSA_Code":"31180","CBSA_Title":"Lubbock, TX","POP2010":290805,"lon":-101.6448,"lat":33.4683},{"CBSA_Code":"31340","CBSA_Title":"Lynchburg, VA","POP2010":246412,"lon":-79.2213,"lat":37.3664},{"CBSA_Code":"31420","CBSA_Title":"Macon, GA","POP2010":232293,"lon":-83.7131,"lat":32.8605},{"CBSA_Code":"31460","CBSA_Title":"Madera, CA","POP2010":150865,"lon":-119.7598,"lat":37.2176},{"CBSA_Code":"31700","CBSA_Title":"Manchester-Nashua, NH","POP2010":400721,"lon":-71.7209,"lat":42.9174},{"CBSA_Code":"31740","CBSA_Title":"Manhattan, KS","POP2010":92719,"lon":-96.5088,"lat":39.341},{"CBSA_Code":"31860","CBSA_Title":"Mankato-North Mankato, MN","POP2010":96740,"lon":-94.1354,"lat":44.1542},{"CBSA_Code":"31900","CBSA_Title":"Mansfield, OH","POP2010":124475,"lon":-82.5425,"lat":40.7731},{"CBSA_Code":"32780","CBSA_Title":"Medford, OR","POP2010":203206,"lon":-122.7291,"lat":42.4314},{"CBSA_Code":"32900","CBSA_Title":"Merced, CA","POP2010":255793,"lon":-120.7137,"lat":37.1929},{"CBSA_Code":"33140","CBSA_Title":"Michigan City-La Porte, IN","POP2010":111467,"lon":-86.7376,"lat":41.5471},{"CBSA_Code":"33220","CBSA_Title":"Midland, MI","POP2010":83629,"lon":-84.388,"lat":43.6433},{"CBSA_Code":"33260","CBSA_Title":"Midland, TX","POP2010":141671,"lon":-101.9911,"lat":32.0897},{"CBSA_Code":"33540","CBSA_Title":"Missoula, MT","POP2010":109299,"lon":-113.9216,"lat":47.0375},{"CBSA_Code":"33660","CBSA_Title":"Mobile, AL","POP2010":412992,"lon":-88.2071,"lat":30.7944},{"CBSA_Code":"33700","CBSA_Title":"Modesto, CA","POP2010":514453,"lon":-120.9952,"lat":37.5581},{"CBSA_Code":"33740","CBSA_Title":"Monroe, LA","POP2010":176441,"lon":-92.2879,"lat":32.6831},{"CBSA_Code":"33780","CBSA_Title":"Monroe, MI","POP2010":152021,"lon":-83.5374,"lat":41.9285},{"CBSA_Code":"33860","CBSA_Title":"Montgomery, AL","POP2010":374536,"lon":-86.4015,"lat":32.3632},{"CBSA_Code":"34060","CBSA_Title":"Morgantown, WV","POP2010":129709,"lon":-79.8035,"lat":39.5264},{"CBSA_Code":"34100","CBSA_Title":"Morristown, TN","POP2010":113951,"lon":-83.3837,"lat":36.1079},{"CBSA_Code":"34580","CBSA_Title":"Mount Vernon-Anacortes, WA","POP2010":116901,"lon":-121.7224,"lat":48.4782},{"CBSA_Code":"34620","CBSA_Title":"Muncie, IN","POP2010":117671,"lon":-85.3973,"lat":40.2279},{"CBSA_Code":"34740","CBSA_Title":"Muskegon, MI","POP2010":172188,"lon":-86.152,"lat":43.2914},{"CBSA_Code":"34820","CBSA_Title":"Myrtle Beach-Conway-North Myrtle Beach, SC-NC","POP2010":376722,"lon":-78.6622,"lat":33.9876},{"CBSA_Code":"34900","CBSA_Title":"Napa, CA","POP2010":136484,"lon":-122.3324,"lat":38.5107},{"CBSA_Code":"34940","CBSA_Title":"Naples-Immokalee-Marco Island, FL","POP2010":321520,"lon":-81.3445,"lat":26.1169},{"CBSA_Code":"35100","CBSA_Title":"New Bern, NC","POP2010":126802,"lon":-77.0782,"lat":35.0938},{"CBSA_Code":"35660","CBSA_Title":"Niles-Benton Harbor, MI","POP2010":156813,"lon":-86.4125,"lat":41.9547},{"CBSA_Code":"35980","CBSA_Title":"Norwich-New London, CT","POP2010":274055,"lon":-72.1029,"lat":41.4876},{"CBSA_Code":"36100","CBSA_Title":"Ocala, FL","POP2010":331298,"lon":-82.0583,"lat":29.2076},{"CBSA_Code":"36140","CBSA_Title":"Ocean City, NJ","POP2010":97265,"lon":-74.8002,"lat":39.1482},{"CBSA_Code":"36220","CBSA_Title":"Odessa, TX","POP2010":137130,"lon":-102.5429,"lat":31.869},{"CBSA_Code":"36500","CBSA_Title":"Olympia-Tumwater, WA","POP2010":252264,"lon":-122.8298,"lat":46.9287},{"CBSA_Code":"36780","CBSA_Title":"Oshkosh-Neenah, WI","POP2010":166994,"lon":-88.6448,"lat":44.0689},{"CBSA_Code":"36980","CBSA_Title":"Owensboro, KY","POP2010":114752,"lon":-87.059,"lat":37.6992},{"CBSA_Code":"37460","CBSA_Title":"Panama City, FL","POP2010":184715,"lon":-85.4665,"lat":30.1439},{"CBSA_Code":"37620","CBSA_Title":"Parkersburg-Vienna, WV","POP2010":92673,"lon":-81.4635,"lat":39.1449},{"CBSA_Code":"37860","CBSA_Title":"Pensacola-Ferry Pass-Brent, FL","POP2010":448991,"lon":-87.1566,"lat":30.6879},{"CBSA_Code":"37900","CBSA_Title":"Peoria, IL","POP2010":379186,"lon":-89.5159,"lat":40.7889},{"CBSA_Code":"38220","CBSA_Title":"Pine Bluff, AR","POP2010":100258,"lon":-91.9476,"lat":34.0778},{"CBSA_Code":"38340","CBSA_Title":"Pittsfield, MA","POP2010":131219,"lon":-73.2062,"lat":42.3711},{"CBSA_Code":"38540","CBSA_Title":"Pocatello, ID","POP2010":82839,"lon":-112.226,"lat":42.6691},{"CBSA_Code":"38860","CBSA_Title":"Portland-South Portland, ME","POP2010":514098,"lon":-70.4695,"lat":43.694},{"CBSA_Code":"38940","CBSA_Title":"Port St. Lucie, FL","POP2010":424107,"lon":-80.4498,"lat":27.2198},{"CBSA_Code":"39140","CBSA_Title":"Prescott, AZ","POP2010":211033,"lon":-112.5548,"lat":34.5995},{"CBSA_Code":"39380","CBSA_Title":"Pueblo, CO","POP2010":159063,"lon":-104.5127,"lat":38.1736},{"CBSA_Code":"39460","CBSA_Title":"Punta Gorda, FL","POP2010":159978,"lon":-81.9139,"lat":26.9051},{"CBSA_Code":"39540","CBSA_Title":"Racine, WI","POP2010":195408,"lon":-88.0611,"lat":42.7473},{"CBSA_Code":"39660","CBSA_Title":"Rapid City, SD","POP2010":134598,"lon":-102.8997,"lat":44.1917},{"CBSA_Code":"39740","CBSA_Title":"Reading, PA","POP2010":411442,"lon":-75.9268,"lat":40.4142},{"CBSA_Code":"39820","CBSA_Title":"Redding, CA","POP2010":177223,"lon":-122.0423,"lat":40.7637},{"CBSA_Code":"39900","CBSA_Title":"Reno, NV","POP2010":425417,"lon":-119.6575,"lat":40.6182},{"CBSA_Code":"40220","CBSA_Title":"Roanoke, VA","POP2010":308707,"lon":-79.9461,"lat":37.2903},{"CBSA_Code":"40340","CBSA_Title":"Rochester, MN","POP2010":206877,"lon":-92.3378,"lat":43.9561},{"CBSA_Code":"40420","CBSA_Title":"Rockford, IL","POP2010":349431,"lon":-89.0413,"lat":42.3334},{"CBSA_Code":"40580","CBSA_Title":"Rocky Mount, NC","POP2010":152392,"lon":-77.7956,"lat":35.9407},{"CBSA_Code":"40660","CBSA_Title":"Rome, GA","POP2010":96317,"lon":-85.2122,"lat":34.2747},{"CBSA_Code":"40980","CBSA_Title":"Saginaw, MI","POP2010":200169,"lon":-84.0528,"lat":43.3303},{"CBSA_Code":"41060","CBSA_Title":"St. Cloud, MN","POP2010":189093,"lon":-94.4732,"lat":45.5863},{"CBSA_Code":"41100","CBSA_Title":"St. George, UT","POP2010":138115,"lon":-113.5064,"lat":37.2813},{"CBSA_Code":"41140","CBSA_Title":"St. Joseph, MO-KS","POP2010":127329,"lon":-94.7857,"lat":39.8348},{"CBSA_Code":"41420","CBSA_Title":"Salem, OR","POP2010":390738,"lon":-122.8964,"lat":44.9034},{"CBSA_Code":"41500","CBSA_Title":"Salinas, CA","POP2010":415057,"lon":-121.2399,"lat":36.2178},{"CBSA_Code":"41540","CBSA_Title":"Salisbury, MD-DE","POP2010":373802,"lon":-75.4681,"lat":38.4177},{"CBSA_Code":"41660","CBSA_Title":"San Angelo, TX","POP2010":111823,"lon":-100.6719,"lat":31.3627},{"CBSA_Code":"42020","CBSA_Title":"San Luis Obispo-Paso Robles-Arroyo Grande, CA","POP2010":269637,"lon":-120.4039,"lat":35.3876},{"CBSA_Code":"42100","CBSA_Title":"Santa Cruz-Watsonville, CA","POP2010":262382,"lon":-122.0099,"lat":37.0576},{"CBSA_Code":"42140","CBSA_Title":"Santa Fe, NM","POP2010":144170,"lon":-105.976,"lat":35.5073},{"CBSA_Code":"42200","CBSA_Title":"Santa Maria-Santa Barbara, CA","POP2010":423895,"lon":-120.0219,"lat":34.7247},{"CBSA_Code":"42220","CBSA_Title":"Santa Rosa, CA","POP2010":483878,"lon":-122.8896,"lat":38.5298},{"CBSA_Code":"42340","CBSA_Title":"Savannah, GA","POP2010":347611,"lon":-81.3016,"lat":32.1312},{"CBSA_Code":"42680","CBSA_Title":"Sebastian-Vero Beach, FL","POP2010":138028,"lon":-80.6067,"lat":27.6934},{"CBSA_Code":"42700","CBSA_Title":"Sebring, FL","POP2010":98786,"lon":-81.344,"lat":27.3437},{"CBSA_Code":"43100","CBSA_Title":"Sheboygan, WI","POP2010":115507,"lon":-87.9457,"lat":43.7211},{"CBSA_Code":"43300","CBSA_Title":"Sherman-Denison, TX","POP2010":120877,"lon":-96.6773,"lat":33.627},{"CBSA_Code":"43340","CBSA_Title":"Shreveport-Bossier City, LA","POP2010":439811,"lon":-93.6692,"lat":32.4889},{"CBSA_Code":"43420","CBSA_Title":"Sierra Vista-Douglas, AZ","POP2010":131346,"lon":-109.7519,"lat":31.8791},{"CBSA_Code":"43580","CBSA_Title":"Sioux City, IA-NE-SD","POP2010":168563,"lon":-96.373,"lat":42.5792},{"CBSA_Code":"43620","CBSA_Title":"Sioux Falls, SD","POP2010":228261,"lon":-96.9893,"lat":43.4994},{"CBSA_Code":"43780","CBSA_Title":"South Bend-Mishawaka, IN-MI","POP2010":319224,"lon":-86.1315,"lat":41.775},{"CBSA_Code":"43900","CBSA_Title":"Spartanburg, SC","POP2010":313268,"lon":-81.8488,"lat":34.8381},{"CBSA_Code":"44100","CBSA_Title":"Springfield, IL","POP2010":210170,"lon":-89.698,"lat":39.8287},{"CBSA_Code":"44180","CBSA_Title":"Springfield, MO","POP2010":436712,"lon":-93.1754,"lat":37.3641},{"CBSA_Code":"44220","CBSA_Title":"Springfield, OH","POP2010":138333,"lon":-83.7844,"lat":39.9189},{"CBSA_Code":"44300","CBSA_Title":"State College, PA","POP2010":153990,"lon":-77.8185,"lat":40.919},{"CBSA_Code":"44420","CBSA_Title":"Staunton-Waynesboro, VA","POP2010":118502,"lon":-79.1275,"lat":38.165},{"CBSA_Code":"44940","CBSA_Title":"Sumter, SC","POP2010":107456,"lon":-80.38,"lat":33.9154},{"CBSA_Code":"45220","CBSA_Title":"Tallahassee, FL","POP2010":367413,"lon":-84.2887,"lat":30.4055},{"CBSA_Code":"45460","CBSA_Title":"Terre Haute, IN","POP2010":172425,"lon":-87.3438,"lat":39.3962},{"CBSA_Code":"45500","CBSA_Title":"Texarkana, TX-AR","POP2010":149198,"lon":-94.2092,"lat":33.4706},{"CBSA_Code":"45540","CBSA_Title":"The Villages, FL","POP2010":93420,"lon":-82.0795,"lat":28.7047},{"CBSA_Code":"45820","CBSA_Title":"Topeka, KS","POP2010":233870,"lon":-95.8021,"lat":39.0438},{"CBSA_Code":"45940","CBSA_Title":"Trenton, NJ","POP2010":366513,"lon":-74.6996,"lat":40.2823},{"CBSA_Code":"46220","CBSA_Title":"Tuscaloosa, AL","POP2010":230162,"lon":-87.7205,"lat":33.1659},{"CBSA_Code":"46340","CBSA_Title":"Tyler, TX","POP2010":209714,"lon":-95.2691,"lat":32.3742},{"CBSA_Code":"46540","CBSA_Title":"Utica-Rome, NY","POP2010":299397,"lon":-75.1761,"lat":43.3356},{"CBSA_Code":"46660","CBSA_Title":"Valdosta, GA","POP2010":139588,"lon":-83.2365,"lat":30.8286},{"CBSA_Code":"46700","CBSA_Title":"Vallejo-Fairfield, CA","POP2010":413344,"lon":-121.9384,"lat":38.2695},{"CBSA_Code":"47020","CBSA_Title":"Victoria, TX","POP2010":94003,"lon":-97.1974,"lat":28.7271},{"CBSA_Code":"47220","CBSA_Title":"Vineland-Bridgeton, NJ","POP2010":156898,"lon":-75.1098,"lat":39.3724},{"CBSA_Code":"47300","CBSA_Title":"Visalia-Porterville, CA","POP2010":442179,"lon":-118.7993,"lat":36.2206},{"CBSA_Code":"47380","CBSA_Title":"Waco, TX","POP2010":252772,"lon":-97.0901,"lat":31.4265},{"CBSA_Code":"47460","CBSA_Title":"Walla Walla, WA","POP2010":62859,"lon":-118.252,"lat":46.2575},{"CBSA_Code":"47580","CBSA_Title":"Warner Robins, GA","POP2010":179605,"lon":-83.64,"lat":32.4093},{"CBSA_Code":"47940","CBSA_Title":"Waterloo-Cedar Falls, IA","POP2010":167819,"lon":-92.4685,"lat":42.5363},{"CBSA_Code":"48060","CBSA_Title":"Watertown-Fort Drum, NY","POP2010":116229,"lon":-75.9206,"lat":44.053},{"CBSA_Code":"48140","CBSA_Title":"Wausau, WI","POP2010":134063,"lon":-89.7586,"lat":44.8979},{"CBSA_Code":"48260","CBSA_Title":"Weirton-Steubenville, WV-OH","POP2010":124454,"lon":-80.7031,"lat":40.388},{"CBSA_Code":"48300","CBSA_Title":"Wenatchee, WA","POP2010":110884,"lon":-120.2636,"lat":47.8181},{"CBSA_Code":"48540","CBSA_Title":"Wheeling, WV-OH","POP2010":147950,"lon":-80.842,"lat":39.976},{"CBSA_Code":"48660","CBSA_Title":"Wichita Falls, TX","POP2010":151306,"lon":-98.4906,"lat":33.775},{"CBSA_Code":"48700","CBSA_Title":"Williamsport, PA","POP2010":116111,"lon":-77.0663,"lat":41.3431},{"CBSA_Code":"48900","CBSA_Title":"Wilmington, NC","POP2010":254884,"lon":-77.9007,"lat":34.4688},{"CBSA_Code":"49020","CBSA_Title":"Winchester, VA-WV","POP2010":128472,"lon":-78.4658,"lat":39.2699},{"CBSA_Code":"49420","CBSA_Title":"Yakima, WA","POP2010":243231,"lon":-120.7401,"lat":46.4594},{"CBSA_Code":"49620","CBSA_Title":"York-Hanover, PA","POP2010":434972,"lon":-76.7307,"lat":39.9232},{"CBSA_Code":"49700","CBSA_Title":"Yuba City, CA","POP2010":166892,"lon":-121.5223,"lat":39.1526},{"CBSA_Code":"49740","CBSA_Title":"Yuma, AZ","POP2010":195751,"lon":-113.9063,"lat":32.769}]};

	//create a lookup table and add in a short name for each metro area
	var lookup = {};
	(function(){
		var large = sel.metros.large;
		var small = sel.metros.small;

		//large metro areas
		for(var i=0; i<large.length; i++){
			//add in a short name
			large[i].CBSA_Title_Short = nameshort(large[i].CBSA_Title);
			lookup[large[i].CBSA_Code] = large[i];
		}

		//small metro areas
		for(var j=0; j<small.length; j++){
			//add in a short name
			small[j].CBSA_Title_Short = nameshort(small[j].CBSA_Title);
			lookup[small[j].CBSA_Code] = small[j];
		}

	})();

	sel.lookup = function(cbsa_code){
		var m = lookup[cbsa_code];

		//create a copy of the data to prevent data mutation by user
		var r = {};
		try{
			r.code = m.CBSA_Code;
			r.name = m.CBSA_Title;
			r.nameshort = m.CBSA_Title_Short;
			r.lonlat = [m.lon, m.lat];
		}
		catch(e){
			r = null;
		}
		finally{
			return r;
		}
	};

	//return an array of metro areas
	//optionally provide a lookup_table to determine if a metro area is in it -- could be used to limit metro areas in menu
	sel.list = function(lookup_table){
		//make a deep copy of the data to avoid mutating the module's data
		var lookup = !!lookup_table;
		var cbsa_mapper = function(m){
			var r = {};
			r.code = m.CBSA_Code;
			r.name = m.CBSA_Title;
			r.nameshort = m.CBSA_Title_Short;
			r.lonlat = [m.lon, m.lat];
			if(lookup){
				r.in_lookup = lookup_table.hasOwnProperty(r.code);
			}
			return r;
		};

		var large = sel.metros.large.map(cbsa_mapper);
		var small = sel.metros.small.map(cbsa_mapper);

		return {large:large, small:small};
	};

	var light_theme = false;
	sel.light = function(){
		light_theme = true;
		return sel;
	};

	sel.setup = function(container, t100){
		if(arguments.length < 2 || !!t100){
			var odata = sel.metros.large.slice(0);
		}
		else{
			var odata = sel.metros.large.concat(sel.metros.small).sort(function(a,b){
				return a.CBSA_Title < b.CBSA_Title ? -1 : (a.CBSA_Title > b.CBSA_Title ? 1 : 0);
			});
		}

		var wrap = d3.select(container).classed("makesans",true);
			wrap.selectAll("*").remove(); //there's no updating here

		var instruction = wrap.append("p").html("Select a metropolitan area: ")
										  .style("display","inline-block")
										  .style("margin","0em")
										  .style("line-height","1em")
										  .style("font-size","0.8em")
										  .style("padding","0px 0px 0px 6px")
										  //.style("color","#dc2a2a")
										  .style("text-transform","uppercase");

		var select = wrap.append("select").style("display","block")
										  .style("margin","0px 0px 0px 0px")
										  .style("line-height","1.65em")
										  .style("font-size","1em")
										  .style("padding","2px 5px 2px 5px")
										  .style("background","transparent")
										  //.style("color","#dc2a2a")
										  //.style("border-color","#fafafa")
										  .style("outline","none");

		this.node = select.node();

		var options = select.selectAll("option").data(odata).enter().append("option");
			options.attr("value", function(d,i){return d.CBSA_Code})
				   .text(function(d,i){return d.CBSA_Title})
				   .style("color","#333333");

		 if(light_theme){
				instruction.style("color","#ffffff");
				select.style("color","#ffffff");
				options.style("color","#333333");
			}

		select.on("change", function(d,i){
			var val = this.value;
			try{
				var m = odata[this.selectedIndex];

				if(m.CBSA_Code!==val){
					throw "ERROR";
				}
			}
			catch(e){
				var m = odata[0];
				sel.update(m.CBSA_Code);
			}

			if(!!sel.onchg){
				sel.onchg(m);
			}
		});
		return sel;
	};

	//update the selection, but don't trigger change event
	sel.update = function(val){
		if(this.node){
			this.node.value = val;
		}

		return sel;
	};
	//the callback executes with the sel object as this
	sel.onchange = function(callback){
		sel.onchg = callback;

		return sel;
	};

	return sel;
}

//metro data-join module v1.0
//metzipper and metro_select.missing() passed all unit tests - 1/18/2017
//code refactored on 1/19/2017 -- need to re-test
//currently handles 0ne-to-one joins, need to handle other cases
//cbsa_Var name refers to the var that holds codes, while cbsa_name_var holds names
function metroData(){
	var md = {};
	var ms = metro_select();
	
	//return an array of metro areas not in metro_list
	//metro_list should be (1) an array of cbsa_codes, or 
	//  				   (2) an array of objects passed to accessor (which should return the cbsa code)
	//this is useful, given some metro data, for determining which metros aren't in the data
	md.missing = function(metro_list, accessor){
		var list = arguments.length < 2 ? metro_list : metro_list.map(accessor);
		
		//create a lookup table from metro_list
		var lookup = {};
		for(var i=0; i<list.length; i++){
			lookup[list[i]] = 1;
		}

		var allmet = ms.list(lookup);

		//what large metro areas are missing from metro_list?
		var large_not_in_list = allmet.large	
								.filter(function(d){
									return !d.in_lookup;
								});

		//what small metro areas are missing from metro_list?
		var small_not_in_list = allmet.small
								.filter(function(d){
									return !d.in_lookup;
								});

		return {large: large_not_in_list, small: small_not_in_list};
	};

	//join metro area data to official id data, including lon/lat
	//cbsa_var is the property name in each element of data that contains the cbsa code
	//optionally provide cbsa_name_var to compare the name in data to official cbsa name
	//data should look like: [{cbsa:xxxxx, var1:xx.x, var2:xx.x, ..., varn: xx.x}, 
	//						  {...}, 
	//						   ...]
	md.zip = function(data, cbsa_var, cbsa_name_var){
		//keep track of join quality
		var num_matches = 0;
		var mismatches = [];
		var in_data_not_in_metros = [];
		
		if(arguments.length < 2){cbsa_var = "cbsa";}

		var zipped = data.map(function(d,i,a){
		  var o = {};

		  o.id = ms.lookup(d[cbsa_var]);
		  o.obs = d;

		  if(!!cbsa_name_var && o.id !== null){
		  	if(o.id.name != o.obs[cbsa_name_var]){
		  		var message = o.id.name + " != " + o.obs[cbsa_name_var];
		  		mismatches.push(message);
		  	}
		  	else{
		  		num_matches++;
		  	}
		  }

		  if(o.id === null){
		  	in_data_not_in_metros.push(d);
		  }

		  return o;
		}).filter(function(d,i,a){
			return d.id !== null;
		});

		zipped.merge_details = {};
		if(!!cbsa_name_var){
			zipped.merge_details.mismatches = {summary: "Of the records that matched, " + num_matches + "/" + zipped.length + " matched exactly, based on CBSA name."};
			zipped.merge_details.mismatches.list = mismatches;
		}
		zipped.merge_details.in_data_not_in_metros = in_data_not_in_metros;
		zipped.merge_details.in_metros_not_in_data = md.missing(data, function(d,i){return d[cbsa_var]});

		return zipped;
	};

	return md;

}

//DEPENDS: d3, topjson
function metMap(container){
  var state_topo = {"type":"Topology","objects":{"states":{"type":"GeometryCollection","bbox":[-179.148909,-14.548699,179.77847,71.365162],"geometries":[{"type":"Polygon","properties":{"STATEFP":"31","STATENS":"01779792","AFFGEOID":"0400000US31","GEOID":"31","STUSPS":"NE","NAME":"Nebraska","LSAD":"00","ALAND":198972429772,"AWATER":1356294775},"arcs":[[0,1,2,3,4,5]]},{"type":"MultiPolygon","properties":{"STATEFP":"53","STATENS":"01779804","AFFGEOID":"0400000US53","GEOID":"53","STUSPS":"WA","NAME":"Washington","LSAD":"00","ALAND":172121026344,"AWATER":12540093192},"arcs":[[[6]],[[7]],[[8]],[[9]],[[10]],[[11]],[[12]],[[13]],[[14]],[[15]],[[16]],[[17]],[[18]],[[19]],[[20]],[[21,22,23]]]},{"type":"Polygon","properties":{"STATEFP":"35","STATENS":"00897535","AFFGEOID":"0400000US35","GEOID":"35","STUSPS":"NM","NAME":"New Mexico","LSAD":"00","ALAND":314161426332,"AWATER":755674004},"arcs":[[24,25,26,27,28]]},{"type":"Polygon","properties":{"STATEFP":"46","STATENS":"01785534","AFFGEOID":"0400000US46","GEOID":"46","STUSPS":"SD","NAME":"South Dakota","LSAD":"00","ALAND":196349394266,"AWATER":3379810671},"arcs":[[29,30,31,32,-1,33]]},{"type":"MultiPolygon","properties":{"STATEFP":"21","STATENS":"01779786","AFFGEOID":"0400000US21","GEOID":"21","STUSPS":"KY","NAME":"Kentucky","LSAD":"00","ALAND":102266253024,"AWATER":2389483091},"arcs":[[[34,35,36,37,38,39,40]],[[41,42]]]},{"type":"MultiPolygon","properties":{"STATEFP":"13","STATENS":"01705317","AFFGEOID":"0400000US13","GEOID":"13","STUSPS":"GA","NAME":"Georgia","LSAD":"00","ALAND":149169759045,"AWATER":4740824950},"arcs":[[[43,44,45,46,47,48]]]},{"type":"Polygon","properties":{"STATEFP":"05","STATENS":"00068085","AFFGEOID":"0400000US05","GEOID":"05","STUSPS":"AR","NAME":"Arkansas","LSAD":"00","ALAND":134769279101,"AWATER":2962525273},"arcs":[[49,50,51,52,53,54]]},{"type":"Polygon","properties":{"STATEFP":"42","STATENS":"01779798","AFFGEOID":"0400000US42","GEOID":"42","STUSPS":"PA","NAME":"Pennsylvania","LSAD":"00","ALAND":115883499352,"AWATER":3396643587},"arcs":[[55,56,57,58,59,60,61]]},{"type":"MultiPolygon","properties":{"STATEFP":"28","STATENS":"01779790","AFFGEOID":"0400000US28","GEOID":"28","STUSPS":"MS","NAME":"Mississippi","LSAD":"00","ALAND":121531969224,"AWATER":3928386321},"arcs":[[[-51,62,63,64,65]]]},{"type":"Polygon","properties":{"STATEFP":"08","STATENS":"01779779","AFFGEOID":"0400000US08","GEOID":"08","STUSPS":"CO","NAME":"Colorado","LSAD":"00","ALAND":268428431772,"AWATER":1175462574},"arcs":[[66,-5,67,68,-25,69]]},{"type":"Polygon","properties":{"STATEFP":"49","STATENS":"01455989","AFFGEOID":"0400000US49","GEOID":"49","STUSPS":"UT","NAME":"Utah","LSAD":"00","ALAND":212885012434,"AWATER":6999936465},"arcs":[[70,71,-70,72,73]]},{"type":"Polygon","properties":{"STATEFP":"47","STATENS":"01325873","AFFGEOID":"0400000US47","GEOID":"47","STUSPS":"TN","NAME":"Tennessee","LSAD":"00","ALAND":106800181941,"AWATER":2352826875},"arcs":[[74,-42,75,-40,76,77,-44,78,-63,-50]]},{"type":"Polygon","properties":{"STATEFP":"56","STATENS":"01779807","AFFGEOID":"0400000US56","GEOID":"56","STUSPS":"WY","NAME":"Wyoming","LSAD":"00","ALAND":251465042238,"AWATER":1861255811},"arcs":[[-34,-6,-67,-72,79,80]]},{"type":"Polygon","properties":{"STATEFP":"18","STATENS":"00448508","AFFGEOID":"0400000US18","GEOID":"18","STUSPS":"IN","NAME":"Indiana","LSAD":"00","ALAND":92790537698,"AWATER":1535887357},"arcs":[[81,82,83,-36,84]]},{"type":"MultiPolygon","properties":{"STATEFP":"02","STATENS":"01785533","AFFGEOID":"0400000US02","GEOID":"02","STUSPS":"AK","NAME":"Alaska","LSAD":"00","ALAND":1477946338495,"AWATER":245391672906},"arcs":[[[85]],[[86]],[[87]],[[88]],[[89]],[[90]],[[91]],[[92]],[[93]],[[94]],[[95]],[[96]],[[97]],[[98]],[[99]],[[100]],[[101]],[[102]],[[103]],[[104]],[[105]],[[106]],[[107]],[[108]],[[109]],[[110]],[[111]],[[112]],[[113]],[[114]],[[115]],[[116]],[[117]],[[118]],[[119]],[[120]],[[121]],[[122]],[[123]],[[124]],[[125]],[[126]],[[127]],[[128]],[[129]],[[130]],[[131]],[[132]],[[133]],[[134]],[[135]],[[136]],[[137]],[[138]],[[139]],[[140]],[[141]],[[142]],[[143]],[[144]],[[145]],[[146]],[[147]],[[148]],[[149]],[[150]],[[151]],[[152]],[[153]],[[154]],[[155]],[[156]],[[157]],[[158]],[[159]],[[160]],[[161]],[[162]],[[163]],[[164]],[[165]],[[166]],[[167]],[[168]],[[169]],[[170]],[[171]],[[172]],[[173]],[[174]],[[175]],[[176]],[[177]],[[178]],[[179]],[[180]],[[181]],[[182]],[[183]],[[184]],[[185]],[[186]],[[187]],[[188]],[[189]],[[190]],[[191]],[[192]],[[193]],[[194]],[[195]],[[196]],[[197]],[[198]],[[199]],[[200]],[[201]],[[202]],[[203]],[[204]],[[205]],[[206]],[[207]],[[208]],[[209]],[[210]],[[211]],[[212]],[[213]],[[214]],[[215]],[[216]],[[217]],[[218]],[[219]],[[220]],[[221]],[[222]],[[223]],[[224]],[[225]],[[226]],[[227]],[[228]],[[229]],[[230]],[[231]],[[232]],[[233]],[[234]],[[235]],[[236]],[[237]],[[238]],[[239]],[[240]],[[241]],[[242]],[[243]],[[244]],[[245]],[[246]],[[247]],[[248]],[[249]],[[250]],[[251]],[[252]],[[253]],[[254]],[[255]],[[256]],[[257]],[[258]],[[259]],[[260]]]},{"type":"Polygon","properties":{"STATEFP":"32","STATENS":"01779793","AFFGEOID":"0400000US32","GEOID":"32","STUSPS":"NV","NAME":"Nevada","LSAD":"00","ALAND":284332114099,"AWATER":2047841189},"arcs":[[261,262,-74,263,264]]},{"type":"Polygon","properties":{"STATEFP":"17","STATENS":"01779784","AFFGEOID":"0400000US17","GEOID":"17","STUSPS":"IL","NAME":"Illinois","LSAD":"00","ALAND":143794894211,"AWATER":6200606440},"arcs":[[265,266,267,-85,-35,268]]},{"type":"Polygon","properties":{"STATEFP":"50","STATENS":"01779802","AFFGEOID":"0400000US50","GEOID":"50","STUSPS":"VT","NAME":"Vermont","LSAD":"00","ALAND":23872224593,"AWATER":1034382802},"arcs":[[269,270,271,272]]},{"type":"Polygon","properties":{"STATEFP":"30","STATENS":"00767982","AFFGEOID":"0400000US30","GEOID":"30","STUSPS":"MT","NAME":"Montana","LSAD":"00","ALAND":376964776680,"AWATER":3868193267},"arcs":[[273,274,-30,-81,275]]},{"type":"Polygon","properties":{"STATEFP":"19","STATENS":"01779785","AFFGEOID":"0400000US19","GEOID":"19","STUSPS":"IA","NAME":"Iowa","LSAD":"00","ALAND":144667481633,"AWATER":1077969087},"arcs":[[276,277,-266,278,-2,-33]]},{"type":"MultiPolygon","properties":{"STATEFP":"45","STATENS":"01779799","AFFGEOID":"0400000US45","GEOID":"45","STUSPS":"SC","NAME":"South Carolina","LSAD":"00","ALAND":77858297564,"AWATER":5074366056},"arcs":[[[279,280,-46]]]},{"type":"Polygon","properties":{"STATEFP":"04","STATENS":"01779777","AFFGEOID":"0400000US04","GEOID":"04","STUSPS":"AZ","NAME":"Arizona","LSAD":"00","ALAND":294204474990,"AWATER":1027559124},"arcs":[[281,-264,-73,-29,282]]},{"type":"MultiPolygon","properties":{"STATEFP":"60","STATENS":"01802701","AFFGEOID":"0400000US60","GEOID":"60","STUSPS":"AS","NAME":"American Samoa","LSAD":"00","ALAND":197749662,"AWATER":1307107436},"arcs":[[[283]],[[284]]]},{"type":"MultiPolygon","properties":{"STATEFP":"39","STATENS":"01085497","AFFGEOID":"0400000US39","GEOID":"39","STUSPS":"OH","NAME":"Ohio","LSAD":"00","ALAND":105831379985,"AWATER":10266305761},"arcs":[[[285,286,-62,287,-37,-84]]]},{"type":"MultiPolygon","properties":{"STATEFP":"01","STATENS":"01779775","AFFGEOID":"0400000US01","GEOID":"01","STUSPS":"AL","NAME":"Alabama","LSAD":"00","ALAND":131173369432,"AWATER":4593984687},"arcs":[[[-79,-49,288,289,-64]]]},{"type":"MultiPolygon","properties":{"STATEFP":"55","STATENS":"01779806","AFFGEOID":"0400000US55","GEOID":"55","STUSPS":"WI","NAME":"Wisconsin","LSAD":"00","ALAND":140269484183,"AWATER":29365442607},"arcs":[[[290]],[[291]],[[292]],[[293]],[[294]],[[295]],[[296]],[[297,298,299,-267,-278,300]]]},{"type":"MultiPolygon","properties":{"STATEFP":"41","STATENS":"01155107","AFFGEOID":"0400000US41","GEOID":"41","STUSPS":"OR","NAME":"Oregon","LSAD":"00","ALAND":248608778890,"AWATER":6190771822},"arcs":[[[-23,301,-262,302,303]]]},{"type":"Polygon","properties":{"STATEFP":"29","STATENS":"01779791","AFFGEOID":"0400000US29","GEOID":"29","STUSPS":"MO","NAME":"Missouri","LSAD":"00","ALAND":178052626882,"AWATER":2487589369},"arcs":[[-279,-269,-41,-76,-43,-75,-55,304,305,-3]]},{"type":"MultiPolygon","properties":{"STATEFP":"37","STATENS":"01027616","AFFGEOID":"0400000US37","GEOID":"37","STUSPS":"NC","NAME":"North Carolina","LSAD":"00","ALAND":125918092834,"AWATER":13472548815},"arcs":[[[306]],[[307]],[[308]],[[309]],[[310]],[[311,312,-280,-45,-78]]]},{"type":"Polygon","properties":{"STATEFP":"40","STATENS":"01102857","AFFGEOID":"0400000US40","GEOID":"40","STUSPS":"OK","NAME":"Oklahoma","LSAD":"00","ALAND":177663043025,"AWATER":3374050771},"arcs":[[-69,313,-305,-54,314,-26]]},{"type":"MultiPolygon","properties":{"STATEFP":"51","STATENS":"01779803","AFFGEOID":"0400000US51","GEOID":"51","STUSPS":"VA","NAME":"Virginia","LSAD":"00","ALAND":102282988386,"AWATER":8503553482},"arcs":[[[315,316]],[[317,318]],[[-39,319,320,321,322,323,-312,-77]]]},{"type":"Polygon","properties":{"STATEFP":"54","STATENS":"01779805","AFFGEOID":"0400000US54","GEOID":"54","STUSPS":"WV","NAME":"West Virginia","LSAD":"00","ALAND":62266577599,"AWATER":489469489},"arcs":[[-288,-61,324,-320,-38]]},{"type":"MultiPolygon","properties":{"STATEFP":"22","STATENS":"01629543","AFFGEOID":"0400000US22","GEOID":"22","STUSPS":"LA","NAME":"Louisiana","LSAD":"00","ALAND":111903220951,"AWATER":23747902252},"arcs":[[[325]],[[-52,-66,326,327]],[[328]]]},{"type":"MultiPolygon","properties":{"STATEFP":"26","STATENS":"01779789","AFFGEOID":"0400000US26","GEOID":"26","STUSPS":"MI","NAME":"Michigan","LSAD":"00","ALAND":146455475554,"AWATER":104031185771},"arcs":[[[329]],[[330]],[[331]],[[332]],[[333]],[[334]],[[335]],[[336]],[[337]],[[338,-286,-83]],[[339]],[[340,-299]],[[341]]]},{"type":"MultiPolygon","properties":{"STATEFP":"25","STATENS":"00606926","AFFGEOID":"0400000US25","GEOID":"25","STUSPS":"MA","NAME":"Massachusetts","LSAD":"00","ALAND":20205425910,"AWATER":7130316321},"arcs":[[[342]],[[343]],[[344]],[[-272,345,346,347,348,349,350,351]]]},{"type":"Polygon","properties":{"STATEFP":"16","STATENS":"01779783","AFFGEOID":"0400000US16","GEOID":"16","STUSPS":"ID","NAME":"Idaho","LSAD":"00","ALAND":214042831252,"AWATER":2397619893},"arcs":[[-22,352,-276,-80,-71,-263,-302]]},{"type":"MultiPolygon","properties":{"STATEFP":"72","STATENS":"01779808","AFFGEOID":"0400000US72","GEOID":"72","STUSPS":"PR","NAME":"Puerto Rico","LSAD":"00","ALAND":8867867645,"AWATER":4923406204},"arcs":[[[353]],[[354]],[[355]],[[356]]]},{"type":"MultiPolygon","properties":{"STATEFP":"12","STATENS":"00294478","AFFGEOID":"0400000US12","GEOID":"12","STUSPS":"FL","NAME":"Florida","LSAD":"00","ALAND":138904831800,"AWATER":31406193924},"arcs":[[[357]],[[358]],[[359]],[[360]],[[361]],[[-48,362,-289]]]},{"type":"Polygon","properties":{"STATEFP":"20","STATENS":"00481813","AFFGEOID":"0400000US20","GEOID":"20","STUSPS":"KS","NAME":"Kansas","LSAD":"00","ALAND":211752682810,"AWATER":1346896791},"arcs":[[-4,-306,-314,-68]]},{"type":"MultiPolygon","properties":{"STATEFP":"33","STATENS":"01779794","AFFGEOID":"0400000US33","GEOID":"33","STUSPS":"NH","NAME":"New Hampshire","LSAD":"00","ALAND":23188026108,"AWATER":1026190438},"arcs":[[[363,364,365,-346,-271]]]},{"type":"MultiPolygon","properties":{"STATEFP":"10","STATENS":"01779781","AFFGEOID":"0400000US10","GEOID":"10","STUSPS":"DE","NAME":"Delaware","LSAD":"00","ALAND":5047105127,"AWATER":1398742998},"arcs":[[[366,367]],[[-59,368,369,370]]]},{"type":"MultiPolygon","properties":{"STATEFP":"48","STATENS":"01779801","AFFGEOID":"0400000US48","GEOID":"48","STUSPS":"TX","NAME":"Texas","LSAD":"00","ALAND":676634395761,"AWATER":19027462392},"arcs":[[[371]],[[372]],[[373]],[[374]],[[-315,-53,-328,375,-27]]]},{"type":"MultiPolygon","properties":{"STATEFP":"27","STATENS":"00662849","AFFGEOID":"0400000US27","GEOID":"27","STUSPS":"MN","NAME":"Minnesota","LSAD":"00","ALAND":206236191895,"AWATER":18924724649},"arcs":[[[376,-301,-277,-32,377]]]},{"type":"MultiPolygon","properties":{"STATEFP":"78","STATENS":"01802710","AFFGEOID":"0400000US78","GEOID":"78","STUSPS":"VI","NAME":"United States Virgin Islands","LSAD":"00","ALAND":347962916,"AWATER":1550265400},"arcs":[[[378]],[[379]]]},{"type":"MultiPolygon","properties":{"STATEFP":"09","STATENS":"01779780","AFFGEOID":"0400000US09","GEOID":"09","STUSPS":"CT","NAME":"Connecticut","LSAD":"00","ALAND":12542452026,"AWATER":1814924099},"arcs":[[[-351,380,381,382]]]},{"type":"Polygon","properties":{"STATEFP":"34","STATENS":"01779795","AFFGEOID":"0400000US34","GEOID":"34","STUSPS":"NJ","NAME":"New Jersey","LSAD":"00","ALAND":19048075783,"AWATER":3543447118},"arcs":[[383,-369,-58,384,385,386,387,-367]]},{"type":"Polygon","properties":{"STATEFP":"38","STATENS":"01779797","AFFGEOID":"0400000US38","GEOID":"38","STUSPS":"ND","NAME":"North Dakota","LSAD":"00","ALAND":178711991307,"AWATER":4398999691},"arcs":[[388,-378,-31,-275]]},{"type":"MultiPolygon","properties":{"STATEFP":"24","STATENS":"01714934","AFFGEOID":"0400000US24","GEOID":"24","STUSPS":"MD","NAME":"Maryland","LSAD":"00","ALAND":25147417568,"AWATER":6983598596},"arcs":[[[-318,389]],[[390]],[[391]],[[-60,-371,392,-316,393,-323,394,-321,-325]]]},{"type":"MultiPolygon","properties":{"STATEFP":"23","STATENS":"01779787","AFFGEOID":"0400000US23","GEOID":"23","STUSPS":"ME","NAME":"Maine","LSAD":"00","ALAND":79885774936,"AWATER":11748561064},"arcs":[[[395]],[[396]],[[397]],[[398]],[[399]],[[400]],[[401]],[[-365,402]],[[403]],[[404]]]},{"type":"MultiPolygon","properties":{"STATEFP":"15","STATENS":"01779782","AFFGEOID":"0400000US15","GEOID":"15","STUSPS":"HI","NAME":"Hawaii","LSAD":"00","ALAND":16634133472,"AWATER":11777724416},"arcs":[[[405]],[[406]],[[407]],[[408]],[[409]],[[410]],[[411]],[[412]]]},{"type":"MultiPolygon","properties":{"STATEFP":"66","STATENS":"01802705","AFFGEOID":"0400000US66","GEOID":"66","STUSPS":"GU","NAME":"Guam","LSAD":"00","ALAND":543558314,"AWATER":934345914},"arcs":[[[413]]]},{"type":"MultiPolygon","properties":{"STATEFP":"69","STATENS":"01779809","AFFGEOID":"0400000US69","GEOID":"69","STUSPS":"MP","NAME":"Commonwealth of the Northern Mariana Islands","LSAD":"00","ALAND":472262566,"AWATER":4644320317},"arcs":[[[414]],[[415]],[[416]],[[417]]]},{"type":"MultiPolygon","properties":{"STATEFP":"44","STATENS":"01219835","AFFGEOID":"0400000US44","GEOID":"44","STUSPS":"RI","NAME":"Rhode Island","LSAD":"00","ALAND":2677759613,"AWATER":1323474145},"arcs":[[[-348,418]],[[419]],[[420]],[[-381,-350,421]]]},{"type":"MultiPolygon","properties":{"STATEFP":"06","STATENS":"01779778","AFFGEOID":"0400000US06","GEOID":"06","STUSPS":"CA","NAME":"California","LSAD":"00","ALAND":403488955894,"AWATER":20477992907},"arcs":[[[422]],[[423]],[[424]],[[425]],[[426]],[[427]],[[-303,-265,-282,428]]]},{"type":"MultiPolygon","properties":{"STATEFP":"36","STATENS":"01779796","AFFGEOID":"0400000US36","GEOID":"36","STUSPS":"NY","NAME":"New York","LSAD":"00","ALAND":122054140553,"AWATER":19242497698},"arcs":[[[429]],[[-387,430]],[[431]],[[432,-273,-352,-383,433,-385,-57]]]},{"type":"Polygon","properties":{"STATEFP":"11","STATENS":"01702382","AFFGEOID":"0400000US11","GEOID":"11","STUSPS":"DC","NAME":"District of Columbia","LSAD":"00","ALAND":158364990,"AWATER":18633403},"arcs":[[-395,-322]]}]}},"arcs":[[[20922,66984],[257,0],[216,-1],[141,0],[384,0],[365,-1],[185,0],[8,-60],[59,-85],[36,-62],[26,-66],[23,6],[18,56],[3,48],[13,12],[40,-31],[23,19],[45,-14],[7,22],[31,2],[24,-25],[3,-42],[22,-44],[41,-13],[5,-47],[16,16],[28,-35],[1,-37],[32,-33],[-6,-31],[30,-131],[12,18],[16,-44],[16,10]],[[23042,66391],[13,-8],[5,-44],[-9,-41],[1,-86],[11,-31],[13,-94],[-10,-17],[3,-56],[22,-66],[-1,-76],[13,-11],[-1,-48],[28,-26],[-9,-82],[26,-131],[-14,-110],[6,-55],[-4,-59],[5,-73],[28,-25],[-4,-47],[20,-10],[1,-115],[-7,-18],[20,-38],[-12,-141],[24,-24],[-12,-23],[6,-77],[-5,-41],[19,-197],[-10,-23],[3,-103],[-14,-51],[13,-64],[14,-21],[7,-86]],[[23231,64173],[2,-69],[27,-10],[-9,-56],[19,-157],[-9,-17],[29,-33],[0,-34],[21,-23],[1,-70],[23,-83],[-6,-85],[29,-44]],[[23358,63492],[-321,1],[-253,1],[-229,1],[-139,0],[-204,-1],[-269,0],[-261,1],[-202,0]],[[21480,63495],[0,1163],[-265,1],[-293,-2]],[[20922,64657],[0,2327]],[[15775,72057],[4,105],[10,72],[12,-52],[2,-75],[15,-15],[-29,-44],[-14,9]],[[15740,73442],[0,40],[21,-41],[-21,1]],[[15735,71959],[23,-44],[-4,-23],[-19,67]],[[15721,73655],[2,20],[27,-91],[2,-35],[-31,106]],[[15719,71881],[7,27],[15,-18],[-8,-28],[-14,19]],[[15716,73484],[8,28],[11,-52],[-14,-28],[-5,52]],[[15715,71820],[13,39],[6,-54],[-19,15]],[[15707,73068],[27,148],[3,55],[15,11],[3,-63],[26,-55],[-7,-32],[-19,18],[-18,-28],[4,-55],[12,-24],[14,-103],[5,-118],[8,28],[-4,66],[41,-73],[7,-94],[-8,-57],[-14,12],[-13,87],[-19,-26],[1,30],[-18,44],[2,93],[-4,53],[-18,-2],[-26,85]],[[15690,73470],[17,-11],[-10,-37],[-7,48]],[[15658,73359],[8,85],[15,6],[-2,-66],[17,-91],[-37,43],[-1,23]],[[15639,73490],[27,-3],[-7,-46],[-20,49]],[[15635,73536],[25,93],[17,4],[38,-59],[-25,-70],[-14,68],[-7,-8],[13,-58],[-6,-18],[-29,7],[-12,41]],[[15624,73618],[17,26],[1,-33],[-18,7]],[[15618,73969],[19,-29],[-17,-6],[-2,35]],[[15587,73496],[6,31],[21,1],[1,-28],[30,-73],[-3,-76],[-8,-12],[-28,44],[-10,90],[-9,23]],[[17306,73966],[-1,-731],[-1,-286],[0,-1978],[-5,-93],[20,-62],[6,-108],[11,-34],[-10,-83],[12,-121]],[[17338,70470],[-83,3],[-218,3],[-179,1],[-97,-2],[-38,-78],[-37,8],[-74,-36],[-13,20],[-15,-23],[-13,-57],[-82,-38],[-57,-73],[-12,-42],[-53,-31],[-29,1],[-15,45],[-21,9],[-15,-35],[-46,-52],[-11,-33],[-14,15],[-39,-9],[-13,-45],[-15,-4],[-9,76],[-34,39],[-17,-14],[-37,39],[-56,-38],[-21,15],[-16,-16],[-11,-45],[-54,-82],[-23,-7],[-18,-35],[-23,1],[-14,32],[-26,3],[-56,46],[-25,45],[-1,144],[-8,34],[3,67],[-8,109],[-25,143],[-59,118],[-14,4],[-32,-51],[-25,2],[-17,41],[1,55],[-14,47],[-19,-12]],[[15491,70777],[-34,9],[-15,40],[-20,-20],[-23,-52],[-9,7],[-21,74],[-10,8],[-15,-52],[5,134],[-3,304],[12,-76],[-2,-100],[7,-145],[15,8],[-11,82],[1,56],[13,-11],[13,31],[0,42],[-18,100],[12,49],[24,31],[-16,53],[-31,-52],[-27,47],[-3,105],[-9,80],[14,-44],[29,63],[41,38],[-30,17],[-20,29],[-3,42],[-26,14],[-8,-24],[6,-88],[-13,-21],[0,193],[-17,226],[-12,22],[-16,115],[-8,206],[-18,191],[-19,89],[-37,81],[-12,84],[-4,149],[-12,90],[11,50],[9,138],[-20,58],[27,8],[19,-24],[52,-96],[32,-35],[4,11],[33,-51],[23,-50],[21,-15],[55,-11],[19,14],[33,-38],[11,19],[35,-38],[49,-3],[24,58],[27,-53],[8,-45],[34,15],[-4,-32],[25,-76],[3,53],[-17,35],[16,67],[20,10],[3,-31],[-14,-34],[18,-61],[-4,43],[14,36],[8,-100],[-16,3],[13,-55],[12,-96],[-3,-30],[-27,-69],[-9,-93],[2,-38],[-16,-4],[11,134],[-6,43],[-3,-60],[-12,-24],[-4,-91],[-34,-123],[-28,-143],[-16,-123],[39,-2],[30,40],[6,41],[-34,-53],[-3,-20],[-27,35],[25,140],[31,127],[47,57],[0,55],[18,95],[22,67],[9,1],[-6,73],[20,-9],[10,-196],[-18,2],[14,-60],[-4,-49],[11,-79],[-18,-32],[13,-54],[-10,-48],[-12,-166],[7,-48],[-10,-36],[-31,35],[19,114],[-29,-52],[-5,-91],[8,-45],[-14,-62],[-17,89],[12,66],[0,71],[-8,50],[0,-87],[-10,-23],[-4,-74],[8,-43],[-7,-45],[9,-65],[5,75],[8,-61],[30,-28],[24,85],[8,79],[14,74],[20,-53],[3,67],[29,34],[-10,155],[-16,110],[21,38],[-20,36],[12,104],[-6,90],[15,47],[9,119],[22,25],[3,58],[-35,90],[-7,43],[3,55],[-9,57],[-17,-2],[-7,-48],[10,-66],[17,-51],[1,-35],[-25,85],[-13,4],[-7,58],[2,77],[18,23],[17,-21],[0,84],[-36,60],[-7,53],[-27,-15],[-3,91],[-10,7],[29,34],[4,-64],[7,39],[6,-38],[19,6],[-10,108],[10,-9],[12,49],[-23,76],[-3,61],[8,29],[-12,40],[-20,-16],[-13,-54],[-6,25],[8,55],[-17,20],[-3,52],[-21,54],[12,21],[-19,52],[17,54],[184,0],[96,-6],[146,5],[237,-1],[256,-1],[173,1],[179,0],[164,0],[160,-2]],[[19531,59999],[291,1],[313,0],[2,-9],[244,4],[153,0],[477,3],[204,2]],[[21215,60000],[0,-582]],[[21215,59418],[-11,0],[0,-518],[-1,-2388],[-6,-1168],[1,-1163],[-226,0],[-335,0],[-292,2],[-138,-2],[2,-94],[-9,-25],[11,-82],[21,-52]],[[20232,53928],[-214,0],[-254,0],[0,-524],[-234,-1]],[[19530,53403],[1,1682],[0,2540],[0,470],[0,1904]],[[20921,69308],[5,5],[0,434],[-2,665]],[[20924,70412],[278,0],[159,0],[222,-1],[195,-1],[191,-1],[335,-3],[285,-5],[269,0],[151,-1]],[[23009,70400],[-5,-127],[-20,-92],[-53,-119],[-4,-45],[25,-99],[10,-75],[14,-54],[18,-3],[35,-59],[10,-69],[1,-674],[-1,-1418]],[[23039,67566],[-40,0],[1,-77],[20,-46],[-2,-110],[-14,-6],[6,-83],[23,-2],[3,-84],[8,-34],[-7,-66],[-13,-16],[2,-106],[-11,-40],[4,-40],[-11,-70],[-19,-72],[1,-73],[32,-88],[11,-86],[-4,-51],[13,-25]],[[20922,66984],[-1,2324]],[[25079,59979],[-14,64],[8,60],[35,143],[27,19],[35,-46],[21,-52],[33,-36],[25,-55],[18,10],[9,91],[-25,153],[11,120],[19,45],[13,-26],[22,58],[56,23],[3,61],[-18,57],[-7,105],[27,87],[10,70]],[[25387,60930],[23,-28],[11,38],[-9,97],[9,39],[21,-56],[41,31],[3,-86],[16,3],[6,64],[-10,40],[8,60],[25,-77],[17,41],[19,-7],[45,-101],[17,-13],[9,-61],[15,19],[11,122],[32,35],[17,59],[17,2],[22,-112],[19,21],[-4,-63],[18,12],[3,63],[22,18],[-2,123],[18,10],[5,42],[30,78],[-6,48],[21,-67],[1,-94],[25,-53],[20,2],[19,-63],[2,36],[22,18],[13,94],[-1,87],[33,149],[15,-23],[23,68],[9,111],[10,32],[24,21],[24,111],[-11,169],[12,33],[42,0],[19,-57],[16,16],[43,90],[24,17],[22,-11],[-4,54],[11,39],[-25,65],[12,79],[-17,75],[21,62]],[[26280,62451],[19,47],[34,-81],[52,44],[4,-65],[35,-61],[20,-132],[6,-101],[39,-41],[26,19],[33,-35],[21,-72],[3,-43],[29,-35],[12,60],[29,27],[15,-32],[28,-20],[21,-72],[15,37],[27,-4],[30,115],[39,38],[7,-33],[6,-154],[13,-38],[21,-6],[31,-97],[6,-61]],[[26901,61655],[-1,-90],[6,-94],[-10,-34],[-7,-111],[24,-80],[22,-130],[-7,-37],[21,-48],[2,-48],[21,-79],[12,-115],[45,-98],[0,-47],[46,-18]],[[27075,60626],[-107,-315],[-41,-47],[-62,-124],[0,-88],[-41,-78],[-3,-103],[-35,-48],[-19,8],[-17,-83],[0,-47],[-17,-4],[-53,-62],[-30,-19],[-43,-55],[-8,-26]],[[26599,59535],[-4,-21],[-83,8],[-154,8],[-84,10],[-98,24],[-82,-12],[-95,8],[-140,31],[-72,4],[-229,-13],[-123,-9],[1,35],[-61,17],[10,-160],[-5,-50],[-78,3],[-270,8],[-32,-9]],[[25000,59417],[11,136],[14,19],[19,-79],[9,6],[19,112],[-12,58],[23,70],[-16,37],[11,46],[10,137],[-9,20]],[[24981,59415],[-15,0]],[[24966,59415],[-9,58],[7,38],[17,-10],[0,-86]],[[26062,57654],[185,4],[172,0]],[[26419,57658],[160,0],[178,15]],[[26757,57673],[-1,-58],[-26,-83],[-10,-3],[-9,-70],[-14,-32],[-8,-105],[34,-102],[15,-4],[30,-124],[19,-34],[28,17],[12,-37],[7,-104],[24,-116],[8,-135],[11,-22],[23,-117],[11,-100],[72,-168],[12,-48],[16,-150],[42,-78],[17,-90],[20,-52],[-8,-39],[7,-78],[24,-79],[-6,-25],[24,-42],[7,-86],[70,-157],[-3,-79],[6,-67],[17,-58],[4,-104],[-4,-131],[15,-61],[18,-17],[32,-115],[-5,-47],[23,-158],[-10,-49],[11,-70],[0,-79],[14,-32],[17,13],[34,-78]],[[27377,54220],[10,-54],[-22,-57],[-18,-95],[-20,23],[8,-78],[-27,-102],[1,-106],[-9,-71],[-26,-17],[-1,-31],[23,-16],[-33,-171],[7,-128],[-39,-160],[-3,-121],[-20,-37],[18,-33],[6,-62],[-12,-109],[1,-117]],[[27221,52678],[-13,19],[-33,-11],[-43,69],[-55,53],[-22,-85],[-3,-108],[11,-72],[-2,-138],[-8,-130],[-34,-5],[-11,76],[2,71],[-11,61],[7,36],[-98,26],[-260,64],[-209,45],[-171,31],[-14,49],[-6,153],[-18,135]],[[26230,53017],[-10,125],[-18,67],[-4,130],[21,307],[-5,90],[-19,89],[-4,191],[17,94],[10,171],[-5,53],[48,150],[-31,71],[6,105],[-6,119],[-29,152],[-5,149],[-17,103],[-33,723],[-47,984],[-25,491],[-12,273]],[[24912,58837],[21,-87],[-3,-50],[-13,27],[-16,-48],[18,-55],[-21,-15],[-9,-53],[-38,-24],[6,-92],[18,12],[2,-42],[-28,-51],[11,-89],[-20,54],[-14,-14],[6,-97],[-10,-71],[-4,73],[-11,12],[-20,-107],[18,31],[11,-32],[-9,-92],[-14,-49],[22,-41],[-10,-46],[14,-61],[-30,-21],[-10,-106],[-23,18],[-5,-54]],[[24751,57667],[17,-52],[0,-51],[-46,-88],[-6,59],[-13,-24],[9,-133],[-20,-18],[2,81],[-9,-20],[0,-96],[22,-7],[-14,-76],[-6,60],[-14,-13],[1,-64],[11,-91],[-12,-62],[7,-64],[-26,-63],[-27,-10],[4,-70],[-26,-46],[-4,-71],[-16,45],[-8,-35],[13,-38],[20,1],[-2,-45],[-24,25],[-10,-66],[21,-31],[-5,-61],[-28,-13],[-28,-51],[22,-58],[-17,-68],[7,-60],[16,-36],[-11,-24],[-30,22],[-3,-65],[32,-45],[-15,-36],[-21,58],[-16,-53],[24,-72],[-27,-41],[13,-77],[-15,-69],[29,-10],[-22,-31],[27,-12],[-8,-109],[14,-92],[1,-88],[-26,-4],[-5,-37],[21,-53],[-12,-70]],[[24512,55349],[-93,3],[-374,11],[-245,4],[-89,-1]],[[23711,55366],[0,621],[-11,27],[-76,-38],[-33,71],[-3,40]],[[23588,56087],[7,1169],[8,873],[-52,1288]],[[23551,59417],[309,0],[276,-1],[235,1],[199,-1],[225,-1],[2,-85],[22,-49],[1,-92],[-22,-88],[-22,-50],[-4,-53],[-16,-29],[-24,-138],[180,6]],[[27479,65793],[52,69],[49,91],[16,67],[10,-32],[53,102],[31,44]],[[27690,66134],[0,-316],[162,1],[294,1],[383,2],[387,-3],[19,-52],[10,-108],[24,11],[26,-85],[-7,-34],[14,-19],[2,-157],[-8,-12],[15,-65],[11,-84],[19,-5],[6,-43],[28,-20],[14,11],[12,-85]],[[29101,65072],[-16,-14],[-24,-80],[-12,-112],[-28,-84],[4,-27],[-16,-53],[-29,-57],[-1,-23],[22,-113],[-22,-117],[-10,5],[-9,-100],[2,-135],[26,-9],[9,-30],[4,-147],[25,-24],[7,-67],[20,-50],[7,-52],[20,-42],[14,-62],[-26,-39],[-14,-53],[-19,-16],[-55,-127],[-3,-82],[-36,-46],[-17,0],[-23,-55]],[[28901,63261],[-24,37],[-33,0],[-23,-36],[-24,-94]],[[28797,63168],[-259,-1],[-322,1],[-128,1],[-199,0],[-120,-2]],[[27769,63167],[-290,0],[0,1068]],[[27479,64235],[0,1558]],[[24751,57667],[358,-1],[230,1]],[[25339,57667],[12,-85],[16,-36],[-16,-499],[-27,-865],[-22,-753],[-39,-1372],[15,-1196],[6,-579]],[[25284,52282],[-23,-59],[-30,58],[-45,-24],[-34,89],[4,-42],[-40,-1],[-81,-98],[-5,82],[-16,-24],[10,-58],[-25,-57],[-7,-77],[-22,-8]],[[24970,52063],[-13,-1],[-21,129],[5,56],[-16,78],[1,53],[-24,74],[-23,170],[5,120],[17,105],[11,175],[-324,-6],[-207,-1],[20,47],[-17,90],[10,90],[-13,86],[33,9],[3,48],[-19,96],[10,37],[7,-71],[12,30],[-11,56],[-1,101],[30,45],[-21,19],[-4,69],[27,-29],[1,106],[9,43],[25,-3],[-3,31],[-22,-12],[7,104],[20,13],[24,65],[-2,50],[32,65],[-7,36],[27,133],[-16,22],[2,-66],[-31,10],[0,74],[33,52],[17,-36],[2,100],[18,50],[-26,34],[11,76],[-36,29],[1,105],[15,-68],[12,21],[-24,56],[21,62],[-31,-29],[-10,65],[26,101],[-27,18],[3,116],[21,53],[-4,102],[-14,4],[-7,-92],[-15,25],[13,96]],[[19530,64656],[279,2],[292,2],[152,-2],[66,-5],[268,1],[211,4],[124,-1]],[[21480,63495],[2,-1552],[0,-1151],[0,-800]],[[21482,59992],[-267,8]],[[19531,59999],[1,619],[0,737],[-5,129],[0,518],[2,473],[1,2181]],[[18139,65812],[42,-6],[179,9],[150,5],[144,1],[51,-3],[269,3]],[[18974,65821],[0,-1168],[152,-3],[146,3],[258,3]],[[19531,59999],[-125,0],[-272,-1],[-8,7],[-380,-4],[-208,0],[-223,-1],[-178,0]],[[18137,60000],[-1,690],[1,269],[0,787],[1,1164],[0,1045],[1,435],[1,1296],[-1,126]],[[24912,58837],[7,0],[7,95],[24,52],[-3,51],[-28,76],[33,4],[15,36],[-22,45],[1,36],[22,-1],[6,40],[-10,71],[2,73]],[[24981,59415],[6,-47],[13,49]],[[26599,59535],[116,-8],[268,2],[102,-1],[3,25],[77,-5],[-9,-28]],[[27156,59520],[-8,-72],[3,-67],[-13,-64],[10,-91],[-24,31],[-32,-70],[-35,-211],[-27,-16],[-5,50],[-21,9],[-52,-86],[-14,-88],[-27,-63],[-15,16],[6,75],[-12,39],[-40,-76],[-13,-91],[-8,32],[-17,-29],[-1,-97],[-21,-78],[-24,18],[-45,-73],[-16,-80],[-11,9],[-31,-63],[-14,-57],[-76,-1],[-31,-52],[-8,-48],[-23,-44],[-13,-105],[4,-61],[-20,-56],[-29,-4],[-6,29],[-19,-50],[-9,-277]],[[26062,57654],[-221,8],[-129,1],[-158,12],[-216,6],[1,-14]],[[18974,65821],[0,840],[-1,2038]],[[18973,68699],[-2,176],[0,438],[76,2],[22,-13],[141,5],[25,9],[100,-3],[180,5],[6,-7],[302,2],[180,0],[74,-6],[323,-3],[4,8],[249,0],[268,-4]],[[25527,65480],[43,-91],[23,-11],[35,17],[67,90],[27,56]],[[25722,65541],[194,0],[229,-1],[139,0],[0,-74]],[[26284,65466],[1,-1042],[0,-571],[-3,-578],[-2,-824]],[[25387,60930],[-20,112],[24,-2],[-7,73],[8,85],[-9,22],[21,22],[10,104],[-15,62],[17,104],[20,-28],[13,102],[17,54],[0,81],[27,42],[-6,31],[16,54],[3,87],[22,40],[6,109],[-14,80],[10,113],[-18,33],[2,84],[-23,63],[-1,29],[23,96],[-9,108],[21,45],[0,621],[2,708],[-1,939],[1,477]],[[402,77118],[18,40],[35,-19],[20,40],[43,26],[23,41],[3,133],[30,-2],[9,-24],[-3,-54],[-18,-37],[5,-54],[-9,-91],[-30,3],[-7,-35],[-24,63],[-22,-9],[-17,-31],[-29,8],[-17,-54],[-10,56]],[[257,77317],[9,35],[19,12],[48,-4],[14,-45],[-6,-22],[18,-33],[21,17],[14,-21],[33,34],[-10,-44],[-17,-2],[-29,-30],[-6,-78],[8,-23],[-22,-15],[-11,-50],[8,-56],[-10,-3],[-5,52],[-36,22],[-8,57],[41,11],[1,63],[-27,9],[-11,44],[-23,28],[-13,42]],[[129,76996],[12,15],[21,-15],[-2,-22],[-28,5],[-3,17]],[[80,76965],[34,-23],[-22,-18],[-12,41]],[[78,77209],[3,40],[18,17],[13,-29],[-1,-63],[-17,-10],[-16,45]],[[41,76744],[21,-2],[5,-52],[-14,8],[-12,46]],[[0,76609],[10,36],[12,-52],[-17,-49],[-5,65]],[[6963,82910],[5,26],[33,14],[0,-36],[-29,10],[-9,-14]],[[6905,82746],[14,51],[26,36],[35,3],[20,-78],[42,12],[-3,-36],[-19,-31],[-43,-5],[-15,10],[-20,-19],[-30,17],[-7,40]],[[6785,82617],[30,111],[36,70],[24,17],[20,-17],[2,-38],[-21,-47],[-47,-53],[-25,-73],[-19,30]],[[6785,83684],[16,12],[11,39],[-6,36],[15,39],[10,61],[28,38],[2,38],[28,8],[23,62],[22,9],[8,25],[70,-16],[-1,-123],[31,-35],[-1,36],[20,48],[-21,54],[4,20],[57,-10],[-4,33],[-52,42],[-16,-5],[-3,125],[29,65],[32,24],[26,-5],[-1,-44],[14,-13],[11,-58],[-5,-70],[8,-13],[9,75],[14,-8],[-11,87],[50,-48],[-8,56],[-35,33],[-22,57],[15,50],[23,-28],[39,-71],[7,15],[37,-66],[2,30],[-60,139],[15,12],[37,-56],[17,14],[11,-18],[24,4],[39,-52],[-20,-36],[-10,-52],[-4,-63],[20,-45],[-1,104],[17,39],[16,-47],[7,60],[33,63],[13,-22],[29,78],[4,-41],[27,-25],[-11,-37],[-24,23],[-2,-37],[16,-20],[-6,-44],[27,14],[-17,-64],[-24,-7],[-6,-53],[1,-88],[26,49],[7,-11],[-28,-91],[12,-7],[23,69],[8,-40],[45,10],[-1,-42],[-38,-87],[-13,-110],[-27,17],[-14,36],[-8,-40],[-19,15],[-11,34],[-26,3],[-6,30],[-50,-3],[5,-40],[23,5],[16,-16],[10,-42],[27,-18],[7,-26],[-9,-75],[-17,-46],[-14,38],[-18,-54],[-8,36],[-46,50],[-46,-34],[20,-27],[23,16],[21,-49],[-37,-54],[-36,10],[11,-74],[28,37],[-2,-24],[35,21],[21,-43],[-12,-28],[-31,-3],[-28,-38],[-23,-13],[3,-46],[-11,-43],[-30,5],[7,88],[-22,-23],[11,43],[-5,25],[27,76],[-3,30],[-23,-22],[-2,-31],[-20,-65],[-18,20],[5,-74],[-22,32],[-26,-8],[-9,-34],[30,-1],[-4,-41],[19,-24],[-17,-78],[-28,9],[3,-28],[-21,-42],[2,-25],[22,19],[-29,-50],[-4,19],[-27,-68],[-2,-26],[-20,-28],[-17,23],[-31,-20],[23,120],[16,13],[7,42],[30,35],[10,33],[-11,24],[-25,-24],[0,48],[27,107],[34,54],[-16,28],[-4,-35],[-17,-13],[-27,-72],[-27,-100],[-12,-2],[-13,-57],[-13,-12],[-5,-40],[-21,-38],[1,52],[-8,48],[-20,44],[-34,30],[2,99],[-4,124],[-17,86],[-26,11],[-18,31],[2,37],[-15,27]],[[6518,81908],[21,29],[12,62],[21,-6],[-7,-50],[6,-71],[-6,-33],[-39,14],[-8,55]],[[6240,82158],[15,10],[8,61],[10,-58],[-13,-10],[3,-50],[-23,47]],[[6225,82310],[3,65],[12,-37],[-6,-53],[-9,25]],[[6079,82741],[17,50],[53,0],[17,-38],[-28,5],[-9,-28],[-50,11]],[[5919,82518],[20,24],[11,-49],[-10,-18],[-21,43]],[[5642,81913],[7,60],[20,18],[15,-21],[-4,-46],[-24,24],[-6,-48],[-8,13]],[[5562,81931],[11,41],[14,-25],[-6,-36],[-19,20]],[[5518,80835],[17,32],[21,0],[-8,-58],[-21,-11],[-9,37]],[[5512,81874],[14,26],[11,-48],[-25,22]],[[5502,81787],[8,63],[13,-8],[-21,-55]],[[5480,80974],[8,31],[21,19],[-6,-44],[19,-55],[-27,-42],[-4,48],[12,-7],[-8,72],[-15,-22]],[[5445,80735],[11,10],[15,-39],[-5,-39],[-18,1],[-3,67]],[[5431,81095],[31,21],[-12,82],[21,-3],[5,-84],[-11,-63],[-12,-2],[2,-42],[-23,2],[15,55],[-16,34]],[[5384,80737],[24,30],[12,-9],[-21,-47],[-15,26]],[[5304,81339],[14,11],[6,-42],[-20,31]],[[5263,80839],[8,41],[40,110],[-7,40],[-22,40],[9,22],[22,-33],[-12,48],[3,30],[26,-66],[5,59],[-10,13],[2,45],[23,-39],[-5,52],[10,-9],[11,94],[11,-24],[-3,-47],[-11,3],[-2,-100],[9,-16],[4,53],[13,5],[-4,-60],[-13,-38],[-17,39],[1,-72],[-20,10],[4,-34],[-33,-69],[-4,-45],[-18,-20],[-1,-62],[-9,-17],[-10,47]],[[5246,85246],[12,31],[2,-65],[-14,34]],[[5238,81446],[24,45],[17,-29],[18,10],[-5,-82],[-22,41],[-24,-22],[-8,37]],[[5212,85245],[11,70],[5,-52],[-16,-18]],[[5188,81324],[7,33],[23,-12],[24,24],[7,-65],[-8,-71],[-14,52],[-24,3],[-15,36]],[[5094,81325],[18,69],[20,31],[18,-12],[5,-46],[-6,-65],[28,104],[14,-39],[-17,-60],[14,-70],[-11,-19],[20,-7],[3,-54],[-11,-53],[-4,30],[-21,-11],[-19,53],[-21,2],[-10,-72],[-16,85],[4,108],[-8,26]],[[5074,81995],[19,14],[14,52],[5,-60],[-38,-6]],[[5033,85131],[7,127],[39,53],[14,-1],[52,84],[-47,-218],[-9,-59],[-23,-33],[-31,-5],[-2,52]],[[4933,81185],[12,27],[20,-7],[-5,-70],[-27,50]],[[4886,81035],[3,46],[15,-34],[-18,-12]],[[4863,81195],[11,40],[33,18],[-7,-64],[-19,-12],[-18,18]],[[4805,81118],[21,44],[28,-49],[25,-17],[1,-23],[-29,-60],[-17,33],[12,68],[-7,25],[-10,-73],[-24,52]],[[4656,80869],[11,6],[21,57],[17,-8],[8,-42],[-1,-69],[-22,-62],[-31,64],[-3,54]],[[4643,80268],[23,-7],[3,-46],[-11,4],[-15,49]],[[4577,90939],[27,47],[18,13],[22,-14],[13,18],[17,-14],[8,-39],[-3,-58],[-15,20],[-11,-21],[-49,0],[-27,48]],[[4539,80282],[7,62],[9,15],[51,-42],[16,-45],[4,-44],[-52,26],[-15,-29],[-20,57]],[[4451,81436],[16,7],[-6,-32],[-10,25]],[[3958,89864],[6,19],[22,-21],[7,-61],[-17,-9],[-15,21],[-3,51]],[[3957,80459],[40,75],[25,18],[20,90],[24,135],[1,44],[33,50],[26,-42],[29,25],[30,61],[13,-12],[34,46],[18,43],[32,19],[33,-14],[34,13],[-1,-16],[32,-100],[9,-66],[-2,-59],[9,-68],[14,-41],[44,20],[17,-44],[10,-80],[-25,7],[11,35],[-47,-1],[-11,56],[-12,7],[-16,-35],[0,-74],[-22,-4],[-23,-47],[-63,28],[-80,-20],[-30,-28],[-32,-60],[-3,-70],[-33,-73],[-55,-37],[-40,15],[-14,21],[-25,119],[-4,64]],[[3934,87743],[7,47],[31,-45],[-30,-24],[-8,22]],[[3934,89811],[5,30],[18,-47],[-19,-11],[-4,28]],[[3881,79904],[19,31],[65,-18],[-11,-45],[-23,-9],[-50,41]],[[3808,79873],[61,20],[8,-32],[-15,-29],[-13,36],[-41,5]],[[3782,79835],[16,30],[10,-27],[-21,-24],[-5,21]],[[3751,80072],[22,58],[37,-2],[-23,-43],[4,-45],[44,-24],[-5,-26],[-22,6],[1,-23],[-36,-53],[-9,29],[11,29],[-11,36],[16,35],[-29,23]],[[3632,79930],[8,63],[28,50],[31,-8],[-2,-39],[13,-21],[8,24],[19,-40],[-19,-24],[19,-17],[15,13],[1,-38],[-26,-30],[-21,18],[-7,-51],[-7,25],[-42,-16],[-1,34],[-17,57]],[[3608,79771],[26,3],[9,-22],[-34,-12],[-1,31]],[[3577,79541],[10,-6],[17,54],[27,28],[-6,-40],[6,-52],[-22,-7],[10,-26],[-25,-36],[-12,26],[-5,59]],[[3257,87017],[13,-14],[32,39],[40,-15],[14,17],[21,-25],[24,10],[0,-20],[25,-2],[16,40],[-10,41],[26,46],[28,17],[13,-22],[3,51],[24,39],[32,-37],[19,27],[49,16],[0,38],[16,-116],[43,-2],[11,25],[55,-60],[-12,-52],[13,-57],[-12,-42],[13,-8],[5,-65],[-13,-36],[8,-44],[12,-13],[-5,-37],[13,-27],[16,14],[-12,-40],[0,-42],[-36,-19],[-12,15],[-31,-34],[-20,4],[-55,-63],[17,-56],[-8,-27],[-22,-1],[-7,36],[-22,44],[-38,42],[-15,-12],[-36,4],[-17,37],[-23,13],[-40,79],[-40,39],[-17,-5],[-33,61],[-7,28],[-16,-5],[-10,77],[-32,99]],[[3147,78985],[29,46],[15,42],[41,17],[24,52],[21,-35],[-3,37],[19,-49],[-1,62],[10,-28],[3,51],[24,-19],[14,16],[-11,56],[33,-20],[-18,51],[-7,78],[15,19],[16,-41],[-1,41],[13,10],[-18,48],[19,37],[30,1],[14,-86],[16,-19],[-13,95],[40,9],[-46,34],[-30,38],[-13,-25],[-23,41],[0,33],[-17,11],[-1,35],[15,23],[8,53],[19,42],[34,33],[14,-10],[24,40],[13,-18],[21,8],[10,-49],[-16,-48],[8,-63],[12,13],[-10,-59],[50,103],[6,78],[15,10],[-4,-58],[25,46],[9,-49],[12,-9],[-9,-59],[-13,-16],[-18,20],[-2,-34],[-29,-78],[-14,4],[3,-38],[-29,-25],[6,-30],[29,10],[18,48],[10,-31],[2,43],[14,-5],[-3,-43],[13,1],[3,-54],[-25,-23],[-14,9],[-9,-45],[-17,19],[-13,-32],[-7,-110],[-22,72],[5,-83],[-7,-43],[-20,28],[-11,54],[0,-68],[8,-61],[-17,-10],[-16,34],[-4,-41],[-12,39],[-35,-16],[-3,-24],[-22,-3],[-9,-32],[-35,-33],[-3,-41],[-40,-16],[-15,-72],[-29,29],[-4,-36],[-47,28],[-17,40]],[[3073,93344],[9,48],[29,41],[121,108],[49,58],[74,105],[90,121],[92,112],[59,61],[21,8],[9,25],[58,69],[61,64],[89,83],[66,53],[108,75],[101,56],[85,13],[72,0],[47,-19],[5,-26],[-52,27],[-3,-15],[27,-15],[6,-70],[-9,-42],[-25,-47],[-8,-112],[15,-47],[-28,-71],[-30,-9],[17,-42],[22,19],[14,-78],[19,-46],[44,-30],[33,34],[48,-1],[15,-22],[47,-6],[43,35],[23,-12],[30,17],[37,-69],[50,26],[21,-36],[28,14],[38,45],[27,-10],[43,-61],[-4,-54],[32,4],[-13,28],[11,39],[-10,10],[38,81],[39,154],[15,20],[41,-6],[9,-40],[38,-11],[25,24],[-2,-55],[22,2],[8,56],[-42,121],[-62,44],[-49,25],[-45,-2],[-59,-52],[-1,-34],[26,-59],[-23,-5],[1,51],[-11,13],[18,121],[2,72],[-9,49],[-20,44],[-35,49],[-7,48],[-20,52],[-40,33],[-45,14],[-5,54],[-29,87],[6,48],[26,11],[12,50],[14,-38],[24,31],[12,-19],[17,-72],[30,-79],[29,-21],[-8,-69],[-8,-20],[1,-68],[31,-69],[8,-38],[37,-58],[32,-32],[29,-43],[16,3],[53,42],[10,51],[-53,4],[-30,72],[-32,45],[-7,26],[-43,81],[10,44],[-4,56],[13,21],[7,74],[17,26],[4,31],[21,25],[19,-20],[24,2],[-1,36],[-27,14],[-29,47],[-9,-29],[-19,52],[-21,7],[-72,-32],[-30,-34],[-36,1],[-39,-19],[-34,17],[-34,61],[-54,-13],[0,-17],[29,-11],[-2,-21],[-57,51],[-117,56],[-55,20],[-23,34],[2,63],[-5,75],[-30,184],[-41,158],[-23,63],[-25,42],[-97,107],[-94,155],[-130,194],[-38,38],[-52,35],[-59,27],[-31,39],[-17,67],[-25,48],[-29,36],[-48,42],[-78,39],[-40,-3],[7,14],[122,86],[18,44],[1,60],[18,52],[13,269],[-10,99],[49,-5],[135,-24],[78,11],[188,64],[35,0],[76,22],[83,75],[25,44],[40,45],[35,62],[25,71],[31,61],[36,91],[22,115],[1,254],[33,190],[14,38],[78,167],[61,171],[40,82],[110,133],[16,24],[71,-27],[50,-13],[44,2],[65,37],[79,66],[65,72],[92,137],[29,63],[59,96],[26,35],[42,74],[28,26],[105,68],[41,12],[52,26],[-46,-32],[-32,-3],[-55,-32],[32,-3],[7,11],[17,-47],[-64,-14],[12,-55],[12,20],[38,-19],[21,20],[22,-8],[4,20],[-32,20],[0,16],[33,-24],[78,-13],[75,16],[8,26],[44,-4],[51,16],[73,51],[74,85],[60,97],[90,210],[43,87],[46,59],[21,17],[10,-66],[25,-6],[10,-27],[32,-3],[59,-30],[-3,-19],[17,-25],[-11,-38],[32,21],[9,27],[9,-27],[41,7],[49,-35],[-1,-30],[16,-47],[-9,-41],[-40,-45],[-12,-46],[-16,0],[-8,-26],[-12,11],[-42,-12],[25,-119],[-3,-25],[45,-7],[30,16],[16,-23],[-2,43],[11,-20],[24,15],[-7,101],[12,2],[30,60],[30,22],[13,-42],[15,29],[-8,32],[-23,37],[34,48],[7,-37],[12,-3],[2,39],[-12,26],[11,31],[44,-42],[43,-54],[51,-84],[-11,-63],[4,-102],[11,-41],[60,11],[34,-32],[-9,-25],[21,-16],[29,25],[22,33],[15,68],[53,14],[79,-4],[61,39],[59,-10],[32,-23],[16,-45],[15,-4],[9,39],[14,-10],[42,10],[50,-28],[48,-36],[9,-31],[-30,-24],[-22,-75],[-27,-28],[4,-64],[11,-30],[22,-16],[9,13],[37,-26],[15,6],[17,-30],[-41,21],[-57,-17],[-27,16],[-12,-20],[11,-29],[49,14],[33,-14],[13,14],[35,-7],[19,22],[45,-20],[28,11],[-21,-61],[5,-21],[-38,-13],[-10,-19],[21,-33],[48,1],[37,9],[77,-45],[14,-32],[28,6],[27,58],[27,37],[27,-9],[19,13],[24,-18],[11,35],[39,6],[30,-27],[14,-59],[48,30],[23,-6],[68,95],[24,-28],[32,26],[23,-16],[33,24],[21,-34],[17,18],[12,-16],[34,-1],[28,-28],[28,-47],[21,9],[27,-34],[19,12],[36,-14],[31,-45],[7,-56],[27,-13],[31,42],[12,-52],[18,51],[33,-34],[33,-15],[1,-16],[32,4],[-7,-47],[-14,-8],[49,-44],[41,5],[64,-32],[12,30],[25,-58],[56,-3],[34,36],[52,-8],[40,17],[74,-18],[106,-50],[5,37],[64,-91],[21,-5],[31,-52],[46,-27],[17,14],[8,-38],[75,-34],[21,23],[56,-18],[17,15],[34,65],[45,6],[11,21],[84,48],[13,24],[19,-13],[18,18],[62,23],[29,-23],[19,5],[29,29],[77,-80],[69,-50],[34,-35],[35,-51],[-2,-16],[29,-29],[3,-23],[26,2],[44,-55],[51,-48],[59,-34],[47,-50],[35,-57],[7,-52],[17,-29],[33,-2],[14,61],[55,-42],[0,-10872],[130,-95],[18,100],[134,-146],[81,181],[171,20],[1,-40],[-33,-271],[47,-113],[66,-75],[26,-22],[11,-112],[11,-50],[173,-371],[111,-243],[29,-299],[-8,-93],[22,3],[51,108],[112,159],[10,23],[68,8],[32,138],[-2,210],[31,-19],[17,19],[19,70],[-1,40],[-31,47],[44,47],[69,28],[130,158],[62,-114],[28,-89],[11,-2],[24,-69],[1,-104],[-20,-54],[29,-47],[-10,-49],[19,-76],[72,-38],[6,-66],[32,-73],[24,0],[28,-109],[-6,-68],[26,-20],[-6,-46],[22,-72],[114,-151],[39,-140],[39,-99],[50,-110],[-22,-50],[33,-133],[46,-141],[28,-175],[58,-182],[31,-161],[28,-99],[27,-135],[53,-180],[33,-155],[-34,-140],[90,-52],[-21,-205],[71,-81],[-9,-61],[11,-57],[8,-119],[71,14],[33,-76],[82,-116],[23,-49],[84,-45],[45,-116],[43,-33],[12,-115],[50,-51],[40,23],[23,-116],[-2,-118],[-16,-107],[-19,-66],[1,-61],[10,-38],[-4,-118],[11,-104],[12,-46],[6,-132],[12,-63],[-33,-101],[-24,-122],[-1,-33],[-42,-167],[-38,-97],[-26,-55],[-25,-33],[8,-27],[-16,-24],[-15,35],[1,69],[-14,18],[-1,-42],[-18,-31],[-22,47],[-6,118],[1,63],[-18,50],[5,71],[36,45],[5,24],[16,-54],[21,24],[-21,11],[-6,48],[-23,-47],[-27,-22],[-22,45],[-6,79],[30,66],[17,61],[20,-8],[-17,157],[12,86],[-9,214],[3,20],[-20,94],[5,31],[-15,-6],[-25,103],[-24,61],[-10,49],[34,63],[-11,13],[-27,-57],[-23,-17],[-13,-40],[29,17],[50,-163],[9,-74],[23,-86],[5,-86],[-2,-77],[-9,24],[2,-60],[-6,-58],[6,-99],[-17,14],[3,-64],[-8,-19],[-3,-79],[-34,-80],[-13,5],[-27,57],[25,49],[12,84],[-1,38],[-25,-7],[10,-63],[-13,-64],[-15,-20],[-34,28],[22,82],[-43,-71],[-41,72],[23,-71],[-21,12],[6,-52],[-6,-42],[-16,-1],[11,-34],[-15,-77],[-19,81],[-11,141],[-2,75],[12,0],[0,48],[30,-67],[-3,31],[-26,64],[31,82],[22,100],[-23,35],[7,66],[-10,43],[15,48],[53,20],[-13,22],[-22,-24],[-24,6],[-3,54],[49,-2],[-17,76],[7,34],[-12,13],[-40,-72],[-41,-10],[4,-29],[34,-24],[1,-35],[-16,-88],[-14,14],[15,-72],[-28,-72],[-30,85],[26,-103],[0,-65],[-9,-45],[-22,44],[-21,18],[-18,44],[-12,130],[-16,72],[21,-31],[7,80],[31,8],[12,-15],[-18,62],[15,47],[-8,38],[8,42],[23,15],[-6,104],[2,129],[57,37],[34,-24],[0,18],[-30,28],[-14,-8],[-35,22],[-2,-50],[-22,-12],[-3,-57],[-13,-20],[-14,58],[-16,-9],[23,-64],[-13,-34],[-22,7],[13,-26],[3,-74],[12,-57],[-12,-33],[-11,51],[-12,-46],[-7,17],[-18,-52],[9,-33],[-17,-15],[-8,38],[10,53],[-20,23],[-6,60],[15,19],[-23,11],[6,41],[-34,21],[-16,-53],[-18,87],[-3,125],[7,28],[25,0],[20,116],[27,16],[16,-51],[-6,-82],[22,-54],[22,1],[-25,51],[-10,54],[7,69],[-7,37],[8,26],[-16,102],[39,-40],[5,-60],[18,-34],[-11,103],[-22,31],[-19,51],[-2,78],[13,50],[-45,-40],[-24,29],[9,80],[-9,48],[11,12],[-32,38],[-41,72],[-7,56],[-20,35],[-19,84],[29,13],[11,-39],[6,42],[-13,63],[3,54],[-13,-35],[0,-54],[-37,22],[-21,38],[-30,8],[-3,56],[-18,-3],[-9,-33],[-47,50],[-21,38],[12,50],[-10,41],[16,55],[14,-25],[26,15],[17,-12],[22,26],[-26,27],[-20,-2],[-27,25],[-6,31],[15,27],[-29,84],[2,79],[11,47],[-23,-26],[-16,19],[-16,58],[8,103],[21,7],[12,-41],[40,-35],[59,-89],[16,-56],[17,-29],[-37,123],[-26,56],[-13,2],[-27,69],[-28,45],[-17,0],[8,48],[-6,35],[1,98],[-8,-9],[1,-52],[-12,26],[5,-57],[-3,-58],[-21,8],[-20,98],[-18,64],[4,39],[20,33],[10,-55],[7,41],[-19,85],[-11,-13],[6,-42],[-28,-42],[-11,13],[-35,90],[-8,69],[-1,107],[8,14],[-7,63],[32,45],[-11,30],[-2,70],[-8,-2],[-4,-81],[-24,-32],[-1,-40],[12,-30],[-12,-52],[-50,-6],[-44,40],[-7,-15],[-23,16],[-27,82],[24,29],[-17,14],[5,55],[-22,-9],[-7,18],[-13,115],[-36,135],[-18,80],[19,5],[-7,25],[3,92],[-15,37],[-10,-27],[0,-68],[-36,145],[-6,166],[-8,91],[-19,104],[-14,37],[-14,82],[4,122],[11,86],[-12,0],[-10,-129],[2,-46],[-14,-3],[-25,35],[-4,-16],[28,-38],[2,-58],[22,-31],[4,-66],[14,-63],[-21,26],[-4,71],[-30,72],[-21,9],[27,-80],[8,-71],[17,-14],[-6,-149],[9,-49],[13,-19],[-4,-30],[27,-125],[3,-74],[25,-114],[-18,6],[24,-156],[18,-167],[2,-52],[-15,-11],[-2,-34],[13,-77],[-10,-6],[-36,50],[-24,11],[-24,92],[-21,183],[-11,15],[3,-51],[12,-50],[-4,-38],[-40,58],[-16,-32],[-29,-3],[-38,-19],[4,102],[-12,-19],[-15,22],[19,55],[-13,63],[11,28],[9,-23],[16,24],[-21,37],[-11,78],[-15,24],[4,31],[-24,80],[20,39],[-15,50],[3,48],[-27,60],[13,-76],[-8,-13],[9,-46],[-14,-101],[1,-31],[-28,4],[-40,76],[-26,22],[-12,99],[-10,41],[-7,-57],[-18,78],[-15,30],[23,-96],[0,-35],[-69,86],[-42,112],[-11,-16],[34,-84],[8,-46],[-36,-23],[-18,-95],[26,83],[76,-31],[23,-40],[29,-14],[33,-83],[-26,6],[40,-54],[11,-37],[-39,-91],[27,12],[22,61],[30,-66],[0,-41],[17,-27],[13,-50],[4,-77],[13,-88],[-19,-32],[-14,1],[-35,-34],[1,58],[-29,0],[2,-77],[-19,19],[-17,-15],[-16,18],[-7,36],[-14,-23],[14,-32],[6,-66],[-15,-41],[-18,1],[-13,69],[13,14],[-30,11],[-3,35],[-14,-2],[3,46],[-18,-20],[4,49],[-34,30],[-22,-33],[-19,50],[-63,94],[-20,44],[-61,90],[-2,48],[-31,73],[-37,70],[2,66],[-6,46],[-36,91],[-46,91],[-28,35],[-76,65],[-77,132],[-44,51],[-77,77],[-77,109],[-67,102],[38,65],[8,32],[2,-52],[22,93],[29,72],[-18,38],[-22,122],[0,66],[22,30],[17,78],[-12,54],[-23,-58],[0,-50],[-30,-34],[-15,-57],[-2,-51],[-111,-106],[-12,-31],[-26,-19],[-101,24],[-59,33],[-25,32],[-130,135],[18,51],[26,3],[8,78],[-18,65],[29,60],[18,18],[1,59],[-9,-55],[-47,-45],[-9,87],[-13,-33],[-25,42],[-10,-7],[52,-151],[-8,-30],[-29,-19],[-25,-34],[-40,0],[-40,44],[-116,66],[-38,8],[-29,24],[-45,11],[-58,-3],[-62,-29],[-137,-31],[-77,-62],[-25,32],[-74,12],[10,19],[49,4],[-19,65],[-53,53],[-3,22],[22,20],[-68,-18],[-24,9],[-50,47],[-47,9],[12,30],[37,16],[-13,23],[-49,9],[-3,25],[45,177],[-23,-7],[-9,-35],[-26,-15],[-19,-41],[-12,-94],[-26,5],[-80,108],[3,28],[-28,34],[-43,20],[-35,-22],[-24,25],[46,65],[27,82],[20,85],[-62,-63],[-14,20],[16,71],[-22,-44],[-4,-32],[-19,-11],[7,62],[14,38],[-61,-81],[-31,-6],[-3,27],[59,101],[4,59],[-32,-74],[-18,25],[-2,-30],[-24,4],[3,43],[-20,-65],[-33,-27],[-29,16],[-6,-15],[-21,60],[31,5],[10,38],[60,21],[23,35],[2,24],[21,11],[-1,23],[-19,15],[-13,-50],[-32,-30],[-4,25],[-16,-27],[-25,9],[-3,42],[-18,-41],[7,54],[-6,25],[-11,-31],[-17,57],[-4,61],[14,-36],[31,-4],[-13,31],[-23,15],[15,79],[27,-19],[-26,52],[11,23],[52,-2],[7,11],[40,-7],[-8,49],[-33,9],[-65,-31],[-13,-60],[-25,-12],[-22,-83],[-32,-52],[-20,13],[20,39],[-6,36],[-17,-60],[3,42],[17,52],[-20,98],[17,34],[-23,8],[-13,-26],[9,-27],[3,-107],[-24,-99],[-15,36],[4,40],[-20,-29],[9,-52],[-32,-51],[-24,12],[0,129],[-14,-63],[-4,-68],[-9,89],[7,79],[12,38],[-18,76],[-2,-42],[-14,-81],[-6,-76],[7,-133],[-18,-13],[-5,37],[-16,27],[6,46],[-27,-76],[20,-30],[-1,-36],[-31,11],[-15,35],[-3,45],[-7,-44],[8,-48],[-32,-33],[-31,17],[-2,25],[15,85],[-2,24],[44,121],[11,62],[-12,3],[52,132],[35,39],[3,17],[-37,-33],[17,71],[-9,9],[-75,-218],[0,-38],[-25,-57],[-9,74],[-16,54],[-43,-67],[-12,-53],[-18,-32],[18,-1],[18,62],[40,40],[2,-30],[-21,-125],[-18,21],[10,-48],[-12,-56],[1,-37],[-21,29],[-6,-16],[20,-35],[-16,-14],[-11,29],[-24,8],[-25,-12],[-32,-45],[15,-10],[26,33],[1,-21],[22,22],[6,-29],[-18,1],[-24,-37],[-15,-41],[-3,-81],[22,59],[9,48],[18,35],[16,-19],[15,32],[9,-15],[-10,-54],[5,-44],[-19,-36],[5,-41],[45,164],[35,0],[11,-23],[4,-89],[-13,-44],[-36,-44],[-16,-70],[-43,55],[-13,-10],[-35,-71],[-18,-51],[7,-22],[36,79],[29,43],[36,-54],[-9,-28],[26,12],[-4,-63],[11,17],[3,87],[12,46],[4,-72],[7,98],[18,16],[20,-92],[14,-19],[-13,-36],[20,-37],[-65,-120],[-14,12],[4,-48],[-38,-68],[-1,48],[-19,-22],[21,-37],[-19,-57],[36,73],[19,11],[-22,-65],[17,26],[11,-14],[-5,-79],[16,105],[17,-25],[-14,-61],[13,-28],[2,63],[11,6],[-13,-113],[-17,-75],[-32,-28],[-14,19],[34,8],[-26,8],[8,27],[21,5],[-31,29],[3,85],[-18,-2],[-11,-74],[6,-15],[-9,-92],[-13,-78],[-27,102],[-1,-71],[-20,-57],[-16,3],[-20,39],[-35,-36],[-13,53],[-20,-20],[-37,13],[0,18],[24,75],[-7,12],[-29,-33],[-29,-129],[-4,-47],[-3,121],[-7,-8],[-6,123],[-7,50],[-19,-8],[-2,-94],[15,-44],[-11,-20],[-10,-70],[-19,5],[-25,-125],[11,-58],[13,-6],[-11,-50],[-2,52],[-15,0],[-9,58],[4,118],[-9,43],[-15,-8],[7,-70],[-10,-23],[-1,-51],[-13,37],[-19,2],[30,-67],[-7,-16],[18,-98],[-33,21],[-21,64],[-13,-1],[-14,36],[-7,-28],[30,-55],[0,-71],[-32,-23],[-25,49],[14,-47],[-11,-70],[12,-15],[-21,-20],[-14,23],[9,-48],[-8,-22],[-18,10],[-1,-36],[-19,-94],[-24,-31],[21,83],[-14,-1],[21,120],[12,89],[16,75],[1,47],[-44,-207],[-8,-9],[-19,-118],[-12,9],[6,37],[-20,38],[22,54],[-9,34],[-10,-12],[-16,-67],[-13,3],[23,-56],[0,-28],[-21,2],[21,-29],[-7,-26],[-37,1],[-9,-57],[-20,-20],[-18,-66],[7,-40],[-15,-38],[-15,-1],[-15,70],[-11,-27],[-36,42],[-26,0],[57,-60],[1,-35],[-18,-25],[-15,23],[-7,-23],[-17,45],[0,-37],[-31,57],[-27,-40],[-1,-37],[-16,14],[-17,-23],[16,-14],[-41,-20],[-12,25],[0,53],[-16,-23],[-26,30],[9,25],[-26,4],[-4,69],[19,53],[25,-6],[16,-38],[-7,43],[-27,45],[7,32],[62,71],[23,-19],[13,19],[26,-24],[-12,45],[9,46],[15,1],[31,62],[24,10],[-5,42],[24,39],[19,85],[35,53],[-46,-8],[-31,-60],[-38,-59],[-24,-16],[-18,-43],[-42,18],[-46,69],[-18,75],[20,127],[10,34],[12,115],[29,99],[27,58],[27,79],[7,43],[5,131],[23,35],[4,140],[5,40],[-20,48],[-8,76],[-13,78],[39,36],[14,33],[43,9],[67,113],[41,86],[84,94],[51,-160],[48,-20],[20,16],[27,81],[24,1],[27,-28],[7,-24],[22,13],[60,-47],[50,-9],[26,-25],[20,-39],[-1,26],[-24,68],[-28,37],[-47,-22],[-4,24],[-32,43],[-33,3],[-36,38],[-31,80],[-32,53],[-27,21],[13,62],[25,3],[32,126],[27,26],[-1,41],[50,40],[33,56],[-32,49],[-30,-12],[-31,-46],[-17,-59],[-18,-16],[-8,-50],[-2,-84],[-19,-35],[-25,19],[-36,7],[-74,-18],[-23,59],[-19,-20],[-11,17],[-9,-37],[-53,-59],[-16,-1],[-36,-75],[-26,-114],[-41,-18],[-9,-23],[-40,1],[-38,-63],[-41,-95],[-8,-46],[26,-122],[-3,-26],[-37,30],[-19,-21],[-36,-66],[-15,-78],[-45,-63],[-19,-77],[6,-66],[19,-28],[-18,-37],[-19,-11],[-12,-75],[-24,-21],[-15,-54],[-20,-4],[-9,26],[-31,-19],[-32,20],[9,-28],[47,-67],[-1,-24],[32,-72],[-7,-72],[-23,-62],[-8,-59],[-27,-41],[-64,7],[-44,-36],[-11,-29],[24,-22],[16,29],[19,1],[9,-28],[-16,-134],[-28,-27],[-2,-19],[-26,-27],[-9,44],[-11,-7],[5,-38],[-11,-11],[-22,33],[9,16],[2,79],[-15,50],[-8,-13],[6,-55],[-7,-75],[-26,-34],[-14,60],[-4,-34],[-21,-6],[6,-22],[18,9],[15,-27],[-7,-60],[-38,-3],[-15,-31],[20,-71],[-6,-34],[-49,-20],[-32,-50],[-11,15],[-11,-51],[41,16],[-45,-64],[-16,-145],[-27,-43],[3,-30],[22,6],[-7,-62],[16,-53],[30,62],[50,-24],[19,28],[23,-3],[27,-81],[24,-27],[10,19],[22,-31],[12,-55],[9,0],[6,-84],[-12,-11],[-12,-110],[-10,-39],[-35,-35],[-10,-61],[-49,-34],[-34,4],[-11,-131],[-34,-5],[-7,-21],[8,-70],[18,-23],[-17,-43],[-37,3],[5,-27],[-18,-5],[-35,-60],[6,-21],[20,13],[-9,23],[39,28],[14,-44],[-29,-29],[15,-20],[-4,-46],[-28,-23],[13,-40],[-14,-16],[-19,33],[7,-80],[-31,54],[-13,49],[6,-95],[-24,-22],[-9,-62],[-21,10],[-1,30],[-22,-8],[-4,-45],[-20,-16],[-22,31],[-36,-41],[-8,-44],[-20,-7],[16,-47],[-6,-38],[-43,-54],[-26,3],[8,-122],[-26,-23],[-3,40],[-18,9],[-21,48],[-15,-5],[-10,-92],[16,-30],[-5,-23],[-41,-31],[2,-104],[-17,2],[-6,36],[-25,-48],[-38,35],[0,-78],[6,-74],[-18,2],[-26,53],[-11,-42],[-29,-25],[-46,-108],[-14,-6],[-2,-44],[54,42],[11,-35],[-7,-51],[-18,-34],[24,-39],[-26,-76],[-15,3],[7,-49],[-21,-40],[-27,-6],[16,-41],[-1,-36],[-33,33],[-12,39],[-18,-15],[10,-32],[-10,-33],[-4,-70],[-8,-15],[-17,79],[-13,-12],[-5,-49],[-21,-25],[-17,-84],[-13,17],[-1,-51],[-17,-30],[-51,114],[-24,-33],[19,-71],[-33,-16],[-12,-61],[5,-36],[18,-4],[8,-36],[-11,-27],[-50,-7],[-18,74],[-14,1],[-16,-35],[-19,-2],[-21,-55],[-11,7],[-21,-41],[-9,-40],[65,42],[17,-4],[5,-75],[-19,-34],[-39,45],[-30,12],[3,-69],[-14,-7],[-42,31],[-22,-36],[-28,-90],[5,-48],[16,12],[16,-34],[12,12],[36,-39],[-4,-27],[-26,-37],[-26,7],[9,-28],[20,-7],[1,-35],[33,63],[-3,-52],[-18,-8],[-34,-57],[-3,61],[-6,-65],[3,-51],[-9,-22],[-6,57],[-12,-6],[9,-52],[-9,-22],[23,-7],[-8,-60],[-18,53],[-3,-65],[-19,72],[-4,76],[21,92],[-28,16],[20,-29],[-25,-75],[-3,42],[-9,-48],[6,-55],[-18,-13],[17,-15],[6,-53],[-15,-28],[-10,56],[-11,4],[-10,-37],[-11,32],[-19,-100],[-24,1],[-2,-45],[-17,39],[-59,-40],[-9,-38],[-9,25],[-15,-24],[0,-68],[-17,20],[-9,98],[-15,-40],[9,-19],[6,-93],[-17,-69],[4,-69],[-16,-27],[-7,-53],[-31,-15],[8,45],[26,31],[-27,28],[22,47],[-14,47],[15,74],[-4,29],[-24,26],[-26,6],[-11,-79],[-11,-2],[-17,39],[-25,-69],[9,-48],[-21,3],[11,-30],[-24,9],[4,-45],[-43,-31],[-37,30],[-6,-28],[22,-32],[-25,-45],[-7,-93],[-19,-23],[-19,153],[-1,-32],[-16,-39],[-14,16],[-14,-34],[30,-12],[-1,-63],[-37,-10],[-11,18],[3,45],[-20,18],[-12,-17],[-16,-83],[-68,-98],[-22,7],[-5,25],[-32,-30],[-14,4],[7,151],[33,118],[-8,41],[-43,1],[-20,-24],[-5,-48],[-18,-53],[3,-136],[-48,-155],[-12,-77],[-35,37],[24,-130],[-30,-62],[-25,48],[8,41],[-19,6],[-14,-57],[10,-54],[-8,-34],[-16,-8],[-9,43],[-27,-32],[-24,42],[-7,56],[33,0],[-28,82],[-3,76],[-32,34],[18,20],[-28,1],[-13,-53],[-3,-49],[20,-16],[17,-65],[-10,-48],[15,-94],[-4,-67],[-10,35],[-17,7],[-11,-53],[-16,-20],[-29,-1],[-20,59],[3,46],[-18,66],[-17,31],[-32,-11],[-9,-81],[19,-17],[27,-50],[-3,-53],[-38,-66],[-6,-32],[-38,-35],[-9,54],[18,21],[-3,92],[32,-24],[-20,38],[2,99],[-9,35],[5,33],[27,60],[33,23],[36,-22],[23,20],[6,72],[-17,9],[31,72],[18,-4],[26,71],[19,-24],[20,48],[-2,81],[-23,-7],[21,50],[37,109],[32,112],[34,55],[27,67],[32,34],[35,84],[65,54],[84,54],[-2,-30],[57,-18],[30,31],[6,27],[23,1],[0,-73],[-8,25],[-33,-44],[2,-47],[13,-10],[1,-47],[34,-87],[36,-20],[5,-43],[9,49],[-30,55],[-2,80],[-7,36],[40,-33],[43,8],[10,-31],[-4,-50],[21,15],[5,-29],[36,-17],[-19,57],[27,20],[-19,22],[-36,66],[-16,10],[-13,68],[-18,3],[25,98],[32,215],[9,41],[34,68],[22,63],[25,24],[33,66],[35,86],[75,86],[44,80],[31,30],[68,107],[25,59],[9,-52],[38,-32],[35,19],[0,49],[-12,51],[-1,58],[10,88],[34,129],[44,142],[21,50],[34,54],[28,24],[23,59],[20,77],[39,80],[25,27],[13,-79],[18,-5],[5,32],[-9,103],[-27,4],[-2,114],[13,102],[13,259],[7,103],[16,65],[27,0],[13,47],[-15,2],[-27,75],[1,137],[23,135],[32,49],[21,80],[25,61],[33,50],[-3,68],[20,60],[-7,55],[10,38],[3,95],[-21,-48],[-10,-78],[-36,-36],[-33,-12],[-28,-47],[-35,-43],[-42,-31],[-31,-46],[-72,-75],[-38,-12],[-33,56],[-4,68],[-9,36],[-52,60],[3,47],[12,21],[8,159],[-13,-2],[-24,-97],[-30,-48],[-12,8],[-8,-89],[5,-36],[-7,-43],[-21,-8],[13,-124],[21,-132],[15,-16],[-28,-101],[-28,-18],[-45,37],[-16,76],[-62,265],[-18,64],[-26,57],[-42,21],[19,64],[-9,43],[-35,-7],[-1,-81],[-19,-16],[5,-43],[-31,-35],[-28,121],[-43,-20],[3,67],[-26,-37],[-4,64],[-19,11],[19,49],[-27,91],[-44,-70],[-67,-124],[-19,-87],[-9,50],[-31,1],[-7,-24],[-54,-86],[-21,-3],[-28,-64],[-1,-71],[-46,-39],[-9,-31],[-19,-3],[-16,-45],[-23,-11],[4,31],[-22,60],[-41,3],[-26,-12],[-28,30],[16,25],[17,-6],[20,21],[11,-37],[16,5],[14,45],[-8,24],[17,28],[15,80],[-9,90],[-1,103],[-15,66],[5,25],[-15,21],[-29,93],[-21,136],[28,133],[21,19],[17,41],[8,54],[14,-11],[11,36],[-47,163],[-3,74],[-13,42],[-11,71],[-26,70],[-14,79],[2,27],[-20,45],[-14,83],[-27,61],[-14,65],[-22,7],[-13,-54],[6,-114],[-19,-57],[-24,-9],[-25,21],[-12,-49],[-33,-31],[-57,-72],[-30,-25],[-70,-37],[-72,-15],[-33,0],[-39,14],[-49,41],[-23,119],[29,29],[-1,22],[-23,47],[-51,55],[-30,104],[2,19],[-43,76],[-8,70],[-12,-17],[-34,10],[-51,73],[-34,94],[24,17],[25,58],[-2,36],[-25,5],[-38,-55],[-20,-5],[-30,21],[-12,52],[10,26],[25,-4],[18,50],[10,-1],[12,38],[14,6],[37,83],[-19,71],[41,38],[7,29],[-26,68],[10,32],[-10,21],[-19,-43],[-37,21],[-10,57],[22,44],[26,-6],[20,18],[-1,50],[-17,-18],[-30,29],[-18,47],[4,28],[-20,10],[-16,34],[-26,-119],[-47,17],[-10,19],[-12,138],[13,72],[-27,3],[-52,41],[-12,72],[1,35],[42,47],[4,32],[-20,52],[-26,24],[-46,-27],[-5,-43],[-13,22],[-11,94],[7,50],[30,3],[83,51],[-25,6],[-44,37],[-26,89],[1,23],[48,16],[33,-2],[53,30],[-19,48],[-20,106],[-2,63],[8,59],[20,83],[8,7],[77,222],[43,133],[29,56],[34,50],[34,8],[23,25],[-14,52],[-6,74],[-8,21],[0,82],[14,14],[-11,18],[20,149],[22,76],[15,23],[34,2],[4,26],[-17,21],[8,27],[-10,36],[49,105],[25,31],[39,18],[36,5],[63,-59],[26,0],[32,-83],[24,-7],[-2,-34],[33,-25],[27,-55],[60,17],[18,14],[20,54],[34,52],[8,61],[43,22],[23,56],[41,120],[21,83],[25,44],[-12,66],[69,-48],[14,-22],[-8,-35],[-29,-25],[50,3],[25,21],[20,-12],[29,27],[34,-19],[21,26],[21,-15],[34,16],[47,37],[8,59],[21,31],[36,86],[30,96],[10,60],[-1,56],[-22,132],[-14,47],[-15,116],[0,130],[-14,57],[-72,160],[-40,40],[-36,-8],[17,40],[1,67],[27,31],[7,-17],[35,-26],[41,-3],[18,11],[-22,44],[35,3],[19,45],[28,41],[3,117],[-33,122],[-24,12],[-25,41],[-30,66],[-4,-37],[-26,-46],[-21,-63],[0,-24],[-42,-29],[-47,30],[-41,-27],[-26,-46],[5,-17],[-51,-12],[-24,-16],[-15,-68],[-87,-103],[-13,-56],[-11,-114],[-30,-33],[-14,-38],[-4,91],[-15,55],[7,35],[-30,73],[-25,0],[-2,50],[-29,30],[2,43],[-18,-3],[-32,-63],[-11,-4],[26,-53],[22,-2],[6,-33],[36,-5],[1,-35],[-21,-23],[-2,-58],[-19,-11],[-21,82],[-37,55],[-59,54],[-78,12],[-33,-25],[-49,15],[-63,-17],[-40,-36],[-82,-86],[-48,-13],[-48,35],[-62,37],[-74,37],[-153,62],[-48,67],[-25,112],[1,71],[14,12],[6,61],[-14,51],[-24,46],[-42,50],[2,41],[-16,42],[-33,42],[-26,85],[14,93],[17,37],[-2,-37],[-16,-20],[-9,-56],[5,-39],[25,-30],[34,-15],[39,18],[29,61],[-3,55],[36,62],[-20,44],[-28,25],[-29,-10],[-51,16],[-67,52],[-100,20],[-48,54],[-36,60],[-34,29],[-5,17],[-62,43],[-14,82]],[[2841,93473],[15,12],[1,-29],[-16,17]],[[2795,78416],[14,17],[0,34],[21,7],[11,32],[-2,43],[12,-8],[17,28],[-2,72],[17,16],[9,71],[-10,17],[3,65],[50,131],[22,-27],[23,28],[4,-23],[26,10],[-7,63],[-17,9],[13,67],[-6,47],[17,57],[31,65],[63,46],[28,-52],[32,-1],[-3,-53],[-16,-43],[5,-62],[-75,-128],[-44,-40],[-16,-39],[-4,-61],[-15,-37],[-29,-103],[-25,-9],[-18,-62],[-32,-21],[-79,-146],[-18,-10]],[[2615,78602],[4,45],[19,17],[5,-44],[-10,-47],[-13,-6],[-5,35]],[[2608,82830],[37,-13],[36,5],[17,-13],[-31,-75],[-26,24],[-2,27],[-20,13],[-11,32]],[[2605,78689],[9,30],[5,-42],[-14,12]],[[2546,78427],[6,30],[28,-9],[5,29],[47,11],[8,-17],[0,-57],[-18,-54],[-10,41],[-23,21],[-29,-47],[-14,52]],[[2515,78506],[9,27],[26,-13],[-2,-42],[-13,-20],[-20,48]],[[2498,78302],[4,71],[32,-19],[-7,-56],[-29,4]],[[2432,83467],[1,35],[32,32],[19,-10],[28,18],[-12,-76],[-22,-24],[-7,-27],[-6,42],[-33,10]],[[2314,78113],[7,85],[18,16],[8,39],[17,22],[28,-31],[2,-26],[-15,-59],[-21,0],[-31,-68],[-13,22]],[[2221,78114],[10,28],[7,-32],[-17,4]],[[2183,78021],[17,55],[16,-42],[-13,-48],[-18,-3],[-2,38]],[[2034,90829],[3,77],[14,94],[14,34],[-5,28],[9,47],[-5,65],[28,-16],[2,-90],[24,-32],[50,-40],[91,-57],[38,-10],[71,110],[29,-1],[19,36],[16,-12],[36,1],[60,-118],[13,-121],[26,-15],[20,-31],[60,-16],[22,-33],[6,-33],[17,-13],[63,-20],[58,-2],[48,-14],[19,-23],[32,-12],[-8,-82],[-28,-82],[-14,-17],[-24,25],[-41,14],[-30,-5],[-45,-29],[-4,-25],[-40,-63],[-12,-52],[14,-58],[-20,-7],[-12,-41],[-34,31],[6,24],[-23,107],[-38,76],[-46,49],[-46,4],[4,25],[-21,82],[-22,45],[-38,50],[-83,62],[-66,8],[-15,-23],[-35,-25],[1,-25],[-44,-60],[-21,23],[-9,-10],[-43,54],[-9,0],[-25,84],[-7,58]],[[1816,77775],[15,89],[47,51],[23,-30],[7,-43],[-65,-96],[-27,29]],[[1695,87356],[7,69],[35,50],[-5,-50],[14,-70],[29,-58],[47,-64],[22,-17],[42,3],[11,-34],[34,-50],[-21,-21],[-9,38],[-49,8],[-27,-21],[-36,52],[-12,34],[-45,100],[-37,31]],[[1679,87540],[16,41],[5,-83],[-21,42]],[[1420,77611],[30,-4],[18,26],[-5,-44],[75,47],[32,-11],[-8,-26],[18,-2],[34,-32],[23,21],[56,-15],[33,0],[-55,-42],[-51,-6],[-15,-21],[-25,10],[-20,-24],[-4,32],[-33,16],[-43,-29],[-26,14],[-15,63],[-19,27]],[[1061,77486],[21,24],[16,-9],[22,27],[0,-35],[27,58],[12,-22],[24,64],[36,-38],[40,37],[7,30],[25,-4],[-14,45],[22,9],[20,-17],[-13,57],[18,2],[16,-25],[31,65],[-4,27],[-31,3],[-26,27],[2,29],[26,-10],[4,64],[41,50],[31,-28],[18,-37],[4,-81],[-19,-78],[-31,12],[-9,-24],[35,-98],[-5,-36],[-9,34],[-24,-40],[-31,33],[-19,-35],[-5,-74],[-13,50],[-29,1],[5,-40],[-44,1],[-8,-36],[-12,25],[-27,16],[-40,-11],[1,-21],[-55,-11],[-36,20]],[[948,77403],[4,27],[22,-16],[-26,-11]],[[912,77394],[22,18],[4,-52],[-26,34]],[[888,77441],[14,13],[8,-37],[-22,24]],[[845,77329],[35,27],[9,-20],[-2,-56],[-42,49]],[[818,77531],[18,65],[26,-14],[15,-43],[-2,-42],[-12,-17],[6,-50],[-44,29],[-7,72]],[[812,77260],[8,61],[21,-36],[28,-8],[-7,-31],[-18,13],[-11,-69],[-3,39],[-18,31]],[[601,77027],[17,85],[8,-6],[7,64],[-13,45],[10,43],[11,-25],[14,27],[10,62],[-13,55],[10,41],[29,-14],[15,48],[16,-1],[-1,-100],[-16,-7],[-6,-55],[35,-31],[5,28],[21,-16],[6,35],[33,-8],[3,-47],[-13,-99],[-44,13],[-39,-81],[-31,3],[0,-64],[-10,15],[-13,-33],[-8,84],[2,45],[-31,-150],[-14,44]],[[12321,84999],[6,5],[27,-125],[-9,-6],[-24,126]],[[12309,84875],[4,46],[19,-105],[2,44],[21,-43],[26,-109],[4,-78],[36,21],[41,-10],[24,-32],[36,28],[10,-46],[-14,-6],[8,-45],[24,-73],[4,-53],[27,-74],[-4,-21],[30,-101],[7,-86],[-5,-57],[23,-64],[2,-52],[-10,-9],[-5,51],[-40,115],[-5,83],[-36,108],[-4,87],[-18,65],[-26,22],[23,-77],[-27,39],[15,-97],[-8,-100],[16,50],[41,-180],[16,-50],[-14,-19],[23,-4],[20,-44],[-1,-58],[30,-152],[-18,54],[-8,-22],[15,-50],[4,-65],[-32,-73],[-22,32],[-7,36],[-7,-48],[-9,54],[-7,-42],[19,-56],[-6,-49],[-9,4],[-2,-46],[-16,-37],[-14,6],[-4,-43],[-32,-17],[6,-36],[-31,-56],[3,-26],[-35,8],[-11,86],[10,58],[-12,36],[-2,51],[23,-13],[-13,61],[30,52],[-20,31],[0,66],[15,6],[11,-38],[21,0],[-15,23],[-31,105],[-10,94],[-17,63],[-16,122],[7,126],[-10,80],[-7,115],[-10,35],[0,89],[-15,83],[-12,21],[-16,136],[-3,70]],[[12288,85065],[21,-39],[-8,-24],[-13,63]],[[12202,85625],[15,-61],[0,-48],[-9,18],[-6,91]],[[12176,83205],[16,-7],[-3,-31],[-13,38]],[[12162,83393],[14,19],[3,-33],[-17,14]],[[12156,83545],[10,27],[19,-4],[5,-34],[-15,-43],[-19,54]],[[12140,83120],[15,-15],[-6,-24],[-9,39]],[[12107,83700],[9,14],[48,-22],[2,13],[-40,25],[18,70],[7,2],[-4,70],[28,56],[19,0],[25,-56],[-5,-21],[-30,-38],[6,-24],[13,41],[35,18],[16,-51],[30,-41],[38,21],[25,-30],[8,-72],[-8,-55],[-20,57],[-13,-17],[1,-66],[19,22],[10,-49],[-5,-39],[17,-59],[4,-98],[13,-86],[-1,-61],[13,-63],[-3,-64],[9,-28],[12,-111],[4,-104],[-3,-90],[-13,8],[10,-102],[1,-202],[-11,-56],[6,-27],[-4,-76],[-13,17],[2,40],[-22,11],[-10,61],[-23,51],[-10,99],[-33,138],[0,50],[20,75],[-30,-42],[-14,-1],[2,104],[-21,-26],[-11,38],[-11,1],[-8,59],[13,17],[-12,26],[-10,-39],[-5,36],[-36,7],[1,51],[25,-4],[8,42],[-1,60],[17,-23],[-17,67],[1,41],[48,61],[-18,-9],[-25,26],[-12,53],[7,53],[-13,26],[22,91],[-36,13],[-25,-18],[-11,56],[-25,83]],[[12096,84873],[22,19],[32,-48],[-4,-19],[-36,3],[-14,45]],[[12056,83542],[13,96],[-9,25],[27,24],[42,-68],[12,-52],[-7,-23],[-17,27],[15,-60],[-1,-30],[16,17],[5,46],[21,-55],[-31,-29],[-1,-76],[-18,-94],[-13,6],[-39,-29],[-10,23],[4,94],[23,27],[1,65],[-22,7],[9,71],[-20,-12]],[[11979,85137],[12,13],[2,-56],[-14,43]],[[11978,84770],[33,44],[0,-54],[-26,-11],[-7,21]],[[11909,84758],[34,-3],[-7,-42],[-27,45]],[[11861,84480],[11,76],[19,2],[28,59],[27,-53],[-24,104],[0,32],[17,3],[17,-77],[26,78],[28,-7],[8,-34],[52,108],[13,3],[13,-54],[66,-78],[-7,-60],[-29,-82],[-2,-72],[11,3],[14,85],[26,45],[-5,35],[13,14],[35,-62],[50,1],[2,-36],[44,-25],[10,-72],[-2,-62],[-22,-42],[-54,66],[17,-67],[25,-21],[3,-30],[22,-33],[-18,-41],[-28,-2],[27,-39],[26,15],[9,-110],[13,-96],[-2,-58],[8,-59],[-15,31],[4,-49],[-44,-10],[-17,11],[-11,37],[-62,107],[-36,49],[-25,46],[-24,-45],[40,-44],[-29,-37],[14,-17],[-5,-36],[8,-75],[-45,-99],[-38,33],[-3,37],[-29,41],[-24,61],[-11,54],[5,45],[-19,-31],[-24,21],[12,22],[-12,22],[-9,59],[24,49],[-9,53],[-15,5],[-2,-46],[-15,24],[-13,88],[-15,-11],[-21,60],[3,33],[-16,6],[-9,33],[4,65],[-4,51]],[[11781,85535],[15,4],[6,-42],[-21,38]],[[11019,86442],[10,24],[13,-24],[-23,0]],[[10961,86290],[16,33],[-5,-67],[-11,34]],[[9672,86952],[27,-15],[-4,-60],[-23,75]],[[9624,86529],[11,70],[47,104],[15,46],[42,23],[-61,-113],[-12,-38],[-42,-92]],[[9327,87194],[30,-4],[15,-49],[-26,-12],[-19,65]],[[9232,87232],[17,13],[37,-24],[19,2],[-1,-34],[-22,-11],[-16,27],[-34,27]],[[9142,87311],[13,61],[18,2],[58,42],[21,35],[40,14],[-8,-53],[-56,-56],[-18,-30],[-34,-29],[-34,14]],[[9032,87215],[32,118],[21,4],[27,-22],[26,0],[-4,-70],[67,28],[9,-23],[3,-56],[-19,4],[-29,-38],[-24,3],[-41,-50],[-35,-68],[-12,2],[-13,48],[47,64],[4,31],[-26,-8],[-12,-30],[-19,17],[-2,46]],[[9008,87723],[6,55],[13,16],[13,-36],[-7,-43],[-25,8]],[[8866,87791],[27,38],[32,2],[-11,-44],[21,31],[-16,-54],[-53,27]],[[8840,87587],[16,15],[-4,-35],[-12,20]],[[8822,87620],[28,9],[-8,-23],[-20,14]],[[8817,87533],[18,5],[-12,20],[10,25],[18,-54],[14,28],[8,-16],[-12,-39],[-40,-12],[14,36],[-18,7]],[[8817,87070],[31,55],[13,0],[1,-32],[-38,-58],[-7,35]],[[8698,86522],[4,59],[15,40],[22,3],[-7,55],[19,39],[20,15],[-11,18],[26,36],[16,44],[34,50],[40,153],[11,-2],[17,54],[-8,41],[2,49],[27,38],[7,-20],[-14,-28],[38,5],[-31,-94],[14,8],[31,46],[10,-22],[-14,-49],[-65,-113],[-44,-124],[-7,-48],[3,-59],[-13,15],[-21,-32],[14,-41],[3,-37],[-21,-34],[-30,10],[-20,-59],[-15,2],[-37,-44],[-15,26]],[[8687,87062],[17,56],[18,-4],[28,35],[-5,19],[-24,-32],[-13,2],[18,144],[27,6],[3,74],[21,0],[1,38],[21,47],[-3,-104],[-17,-4],[-2,-48],[10,-27],[-22,-21],[17,-16],[3,-30],[-28,-114],[5,-30],[-17,-102],[-29,69],[-25,16],[-4,26]],[[8672,87613],[11,20],[22,-7],[16,-50],[-25,-42],[-18,36],[-6,43]],[[8662,86712],[32,82],[3,24],[23,45],[9,-33],[-23,-88],[-37,-44],[-7,14]],[[8635,87135],[15,49],[25,34],[7,-16],[-11,-110],[-28,5],[-8,38]],[[8636,86768],[21,109],[16,28],[13,51],[4,-42],[13,9],[6,-32],[-49,-85],[-16,-52],[-8,14]],[[8609,86699],[29,45],[7,-13],[30,85],[5,-13],[-28,-85],[-15,-20],[-28,1]],[[8041,88082],[17,48],[16,-16],[-20,-36],[-13,4]],[[7998,86039],[21,15],[-5,-33],[-16,18]],[[7905,85978],[24,84],[-2,31],[16,-12],[8,-48],[-20,-72],[-11,-18],[-15,35]],[[7695,85778],[28,-18],[-2,-27],[-25,26],[-1,19]],[[7640,85742],[24,2],[-6,-26],[-18,24]],[[7596,85788],[17,26],[8,-48],[-25,22]],[[7593,84673],[5,54],[11,26],[14,-18],[-6,-75],[-13,-23],[-11,36]],[[7540,87172],[21,31],[15,106],[-6,32],[15,29],[25,-37],[-19,-29],[-22,-74],[7,-28],[-36,-30]],[[7477,84157],[20,45],[6,-15],[-26,-30]],[[7475,83702],[8,36],[10,-16],[-18,-20]],[[7464,85509],[9,-4],[8,45],[18,-17],[-3,-29],[-32,5]],[[7385,86963],[6,22],[15,-27],[0,-77],[-21,82]],[[7324,84393],[3,16],[36,18],[-5,-53],[-12,-23],[-22,42]],[[7168,84513],[24,52],[11,48],[10,-2],[25,-63],[21,0],[15,26],[-30,-8],[-20,62],[12,67],[33,-26],[14,31],[-26,46],[15,52],[36,-37],[29,17],[-26,22],[-8,31],[11,13],[35,-32],[-33,104],[22,-3],[19,65],[22,24],[33,-25],[-31,56],[-4,72],[27,60],[15,-36],[18,51],[11,-29],[10,33],[17,-9],[-25,-157],[-20,-29],[-15,-66],[18,-15],[-5,-55],[26,-13],[19,80],[11,16],[28,-77],[17,46],[8,-40],[-12,-81],[0,-89],[16,106],[3,-42],[10,19],[13,76],[7,-25],[0,-85],[-10,-62],[-9,2],[-14,-63],[-33,26],[-2,42],[-13,39],[-16,-5],[6,-75],[-10,-17],[17,-32],[-36,-18],[-12,21],[-37,-37],[2,84],[-9,11],[-3,-75],[-14,-71],[-22,-13],[-13,28],[7,-57],[-7,-38],[-24,-2],[-6,-25],[-44,21],[-23,36],[-47,37],[-19,-14],[-15,26]],[[7125,86044],[43,45],[8,-4],[14,-63],[-6,-22],[-42,-21],[-17,65]],[[99917,77439],[15,3],[13,37],[22,7],[31,-61],[-11,-74],[-34,-41],[-25,29],[-13,47],[2,53]],[[99678,77037],[5,24],[26,-29],[19,2],[27,-23],[5,-34],[23,-39],[21,-63],[31,-41],[11,-58],[44,-10],[21,-33],[-56,-22],[-12,35],[-44,79],[-30,101],[-30,43],[-17,-8],[-22,25],[-22,51]],[[99628,77434],[9,11],[31,-41],[-14,-58],[-10,-4],[-16,92]],[[99570,77259],[19,-7],[18,-56],[-19,4],[-18,59]],[[99530,77498],[7,21],[17,-21],[3,-35],[-17,-19],[-10,54]],[[99282,77339],[24,26],[15,51],[42,35],[3,53],[16,97],[11,18],[20,-53],[-20,-88],[-18,-44],[19,-28],[-13,-44],[-42,16],[-17,-30],[-4,-70],[-36,42],[0,19]],[[98911,77891],[26,-13],[-15,-29],[-11,42]],[[98408,78314],[22,-7],[3,-26],[-25,33]],[[98210,77931],[25,58],[23,-6],[26,66],[41,6],[-22,-77],[10,-104],[-21,0],[-17,52],[-35,-19],[-30,24]],[[97961,78538],[46,87],[33,10],[100,-20],[29,-62],[24,-15],[35,-95],[-2,-20],[-31,-6],[-21,39],[-17,-71],[-13,-13],[-34,15],[-26,-41],[-27,32],[-12,40],[-3,62],[-32,56],[-35,-23],[-14,25]],[[13419,81377],[10,19],[5,-48],[-9,-29],[-6,58]],[[13346,81038],[10,44],[10,-30],[-6,-51],[-14,37]],[[13276,80875],[6,33],[40,20],[25,27],[-1,-82],[15,-22],[-30,-61],[-12,-7],[-43,92]],[[13242,80890],[9,26],[15,-17],[-15,-29],[-9,20]],[[13234,81005],[16,49],[-2,43],[13,-9],[4,36],[-12,41],[-8,64],[10,55],[35,-80],[14,-9],[20,-46],[-11,-8],[-2,-87],[4,-56],[-9,-34],[-29,1],[-6,52],[-26,-73],[-11,61]],[[13215,81948],[21,65],[14,-70],[-35,5]],[[13177,81562],[28,21],[-2,-32],[-22,-27],[-4,38]],[[13117,82158],[10,67],[14,-93],[-19,-36],[-5,62]],[[13001,82778],[22,43],[-1,-25],[-21,-18]],[[12979,82565],[16,59],[28,-38],[-25,-58],[-19,37]],[[12905,80859],[27,23],[30,-72],[-18,-34],[21,-72],[-6,-45],[-12,20],[-8,57],[-27,61],[-7,62]],[[12888,80921],[19,6],[-4,-52],[-15,46]],[[12875,81031],[11,33],[-6,-64],[-5,31]],[[12874,82265],[5,23],[21,-40],[-14,-17],[-12,34]],[[12856,82345],[12,1],[19,-46],[-15,6],[-16,39]],[[12855,82376],[1,22],[20,-13],[-2,-31],[-19,22]],[[12838,82528],[27,109],[28,9],[13,-17],[30,17],[25,-44],[1,-35],[-15,-44],[6,-85],[-20,-25],[-40,-28],[-24,74],[-30,41],[-1,28]],[[12834,82223],[21,35],[7,-72],[-22,-4],[-6,41]],[[12767,81438],[22,37],[-2,-52],[-20,15]],[[12731,81575],[0,21],[23,24],[16,-8],[9,-50],[-22,-89],[-14,26],[-12,76]],[[12703,80773],[12,-63],[-2,-49],[-8,35],[-2,77]],[[12684,81475],[23,51],[3,41],[26,-49],[-5,-60],[-29,-9],[-18,26]],[[12675,81679],[8,23],[4,66],[19,-39],[-22,-100],[-9,50]],[[12665,81308],[13,20],[6,46],[-16,15],[29,63],[3,-20],[40,13],[1,-45],[-42,-49],[-13,-126],[-21,83]],[[12630,82062],[32,130],[11,21],[40,12],[-30,13],[-3,49],[15,-22],[13,31],[-27,73],[-8,77],[2,51],[14,-19],[-6,54],[58,-35],[4,10],[56,-1],[36,-105],[10,-82],[-10,-77],[-18,-5],[12,-68],[22,11],[12,-22],[11,27],[2,-43],[24,12],[62,-145],[41,-146],[-6,-26],[13,-49],[-3,-122],[18,65],[10,-31],[16,-125],[28,-26],[13,-45],[-7,-29],[-15,40],[-65,83],[-8,36],[-25,-18],[13,-12],[-14,-73],[-25,-48],[30,22],[2,35],[40,11],[5,-47],[22,5],[11,-41],[-39,-13],[11,-47],[8,25],[23,13],[6,-34],[28,-69],[15,-80],[-18,-37],[-21,14],[-9,-52],[21,24],[24,-30],[16,88],[15,-17],[4,-99],[-17,-67],[10,-17],[-23,-27],[-2,-45],[-38,-45],[24,-16],[27,52],[15,8],[7,-92],[-1,-111],[-22,35],[2,-28],[19,-24],[6,-55],[-14,-24],[-3,-106],[-43,0],[-9,39],[-33,-7],[-16,43],[2,31],[-38,-8],[18,62],[22,-13],[11,18],[-38,90],[-18,-17],[-2,42],[-24,45],[13,117],[11,34],[-12,30],[-14,-86],[-1,68],[12,65],[-25,83],[12,-75],[-11,-37],[-15,12],[-30,56],[26,-74],[16,-104],[-4,-41],[-17,-28],[-38,56],[8,61],[-20,55],[10,46],[-13,0],[2,41],[-21,-9],[-9,20],[13,26],[-33,15],[17,-100],[-8,-9],[21,-45],[1,-44],[-15,-6],[30,-75],[8,-97],[20,-41],[29,-90],[18,-105],[-10,-42],[13,-20],[-15,-14],[-5,26],[-27,5],[-17,100],[-27,54],[-8,57],[-38,90],[-3,118],[-17,42],[10,58],[-11,37],[7,88],[26,31],[-31,-4],[-6,-55],[-24,-18],[-13,28],[-20,-4],[-3,48],[14,6],[-12,55],[31,32],[13,-21],[1,-46],[14,-14],[4,38],[-15,51],[18,34],[18,-24],[0,37],[20,4],[-14,86],[2,47],[-17,25],[11,48],[-12,16],[-29,-3],[-38,68],[12,24],[-9,51],[24,46],[-23,-9],[-26,26],[9,-35],[-12,-46],[-32,42],[-16,64],[12,46],[21,16],[26,-28],[1,-27],[32,-7],[14,35],[-12,37],[3,45],[-40,65],[9,62],[-6,40],[-15,-55],[-15,-15],[-13,-55],[-45,0],[-4,40]],[[12628,81468],[15,65],[15,25],[-12,14],[26,28],[23,-24],[-1,-39],[-13,-15],[-10,-61],[-17,37],[-26,-30]],[[12613,83594],[6,46],[18,-13],[-24,-33]],[[12594,82000],[18,43],[7,-9],[3,-74],[-8,-27],[-16,28],[-4,39]],[[12518,84319],[7,-2],[24,-108],[-2,-33],[-21,74],[-8,69]],[[12500,84392],[23,-20],[7,-35],[-17,-5],[-13,60]],[[12476,82011],[28,22],[2,-43],[23,30],[16,-1],[-54,-106],[-9,30],[8,29],[-14,39]],[[12461,83089],[29,82],[7,0],[37,-71],[2,42],[-18,22],[-12,43],[28,-27],[5,47],[27,-41],[13,-111],[8,7],[12,-81],[-4,-31],[16,-24],[6,44],[-3,68],[31,-23],[-16,73],[-15,7],[-8,53],[4,46],[-39,97],[4,51],[37,33],[77,-47],[54,-60],[61,4],[32,-24],[23,-59],[22,-158],[26,-13],[16,-44],[7,-50],[33,-51],[6,-60],[16,-39],[-30,-35],[-25,-55],[-22,-14],[-49,19],[-25,22],[-12,-61],[-22,-34],[-14,34],[-28,-4],[-5,25],[-18,-8],[7,-32],[-26,-28],[-41,-1],[-9,92],[6,45],[-13,26],[13,16],[-5,34],[-15,-69],[-16,-3],[-11,57],[-19,9],[17,-52],[-13,-70],[-11,-17],[19,-20],[7,-55],[-20,11],[9,-50],[-16,-24],[23,-30],[5,-31],[-13,-55],[-21,82],[-7,-36],[11,-15],[16,-96],[-21,-17],[8,-69],[-9,-64],[-16,-6],[-8,37],[6,63],[-10,53],[5,41],[-7,74],[-8,-160],[-13,-18],[-15,21],[8,-49],[16,7],[0,-112],[-6,-55],[-13,25],[3,38],[-19,16],[7,40],[-14,31],[15,20],[-16,18],[-3,119],[16,21],[-23,7],[8,30],[-6,45],[16,64],[-5,38],[11,40],[19,-107],[-3,61],[9,25],[-21,97],[-26,41],[-2,84],[-17,24],[-11,67],[3,38],[-9,90]],[[16479,65813],[59,4],[304,-7],[55,4],[175,4],[236,1]],[[17308,65819],[275,-2],[280,-2],[63,7],[58,-9],[155,-1]],[[18137,60000],[2,-939],[-7,-18],[-24,-180],[-24,-10],[-37,149],[-38,-15],[0,23],[-34,-8],[-34,-68],[8,-66],[-5,-60],[22,-119],[-12,-29],[5,-230],[10,-47],[-7,-141],[23,-191],[6,-218],[-20,-42],[11,-32],[-8,-85]],[[17974,57674],[-114,384],[-174,576],[-162,524],[-66,212],[-143,453],[-139,433],[-105,323],[-154,464],[-145,434],[-107,313],[-186,537],[-1,263],[2,851],[0,633],[-1,434],[0,1305]],[[24442,63932],[12,16],[2,140],[-10,35],[16,78],[28,34],[17,-2],[17,38],[8,165],[27,113],[10,19],[6,128],[-1,72],[-12,69],[-15,12],[-20,88],[10,70],[8,127],[21,27],[12,-15],[20,37],[57,11],[17,56],[37,15],[13,49],[20,26],[8,170],[35,80],[10,121],[3,112],[-6,46],[0,86],[-13,42],[-51,85],[-11,61],[4,52],[-17,71],[-49,111],[3,34]],[[24658,66411],[159,0],[113,-4],[188,-10],[244,0],[88,-4]],[[25450,66393],[-1,-119],[-8,-103],[9,-109],[33,-154],[20,-269],[24,-159]],[[25079,59979],[-17,-2],[-18,97],[-14,7],[14,-63],[-9,-27],[-24,55],[0,54],[-24,132],[1,60],[-16,40],[7,54],[15,15],[4,61],[-26,161],[0,57],[11,2],[-11,64],[1,52],[-20,32],[-43,156],[-29,68],[-16,-42],[-20,58],[8,59],[-15,-8],[-37,115],[-31,59],[-31,118],[-6,79],[7,111],[15,58],[10,118],[19,82],[2,77],[-8,54],[25,101],[3,59],[-38,82],[-17,5],[-39,51],[-26,-108],[-11,-7],[-22,65],[-14,154],[8,37],[-12,189],[-19,68],[-68,162],[-17,105],[-14,8],[-10,69],[-15,29],[-36,119],[2,62],[-24,108],[7,43],[-20,141],[-4,107],[1,126],[12,123],[12,42]],[[29478,69324],[139,7],[149,-15],[225,11]],[[29991,69327],[-10,-34],[12,-88],[-17,-70],[-5,-66],[-16,-45],[12,-109],[15,-83],[-18,-38],[6,-59],[-20,-38],[-16,-70],[-30,-35],[0,-31],[-61,-41],[-10,-57],[1,-95],[9,-67],[0,-63],[-23,-97],[-1,-88],[-19,-73],[1,-60],[-14,-66],[-19,-53],[-7,-118],[-13,-25],[-7,-77],[5,-23],[-9,-152],[5,-58],[-11,-64],[-6,-130],[6,-70],[-9,-39],[6,-49],[-8,-45],[-17,-26],[3,-36],[-9,-77],[12,-103],[15,-43]],[[29724,66666],[-97,11],[-127,11]],[[29500,66688],[-4,102],[6,495],[3,310],[-16,115],[-19,0],[-7,-65],[-9,77],[22,160],[-12,59],[5,60],[-9,68],[-1,110],[-7,21],[13,168],[22,99],[-6,114],[10,65],[-3,84],[-21,103],[5,178],[8,73],[-12,39],[11,89],[-1,112]],[[17580,73968],[153,0],[313,1],[329,-3],[333,0],[310,-1],[125,1],[261,2],[353,-1],[294,0],[224,0],[151,0],[158,0],[201,0],[138,0]],[[20923,73967],[0,-466],[2,-858],[-1,-604],[0,-1627]],[[18973,68699],[-21,23],[-3,43],[-25,54],[1,53],[-31,127],[-14,27],[-9,-51],[-20,-4],[5,-33],[-16,-86],[3,-79],[-20,30],[-32,-2],[-35,-59],[-11,61],[-31,-31],[-15,12],[-21,-30],[-50,55],[-20,-46],[-8,-94],[-24,37],[-52,7],[-16,21],[-18,-22],[-15,-73],[6,-51],[-8,-24],[-45,108],[-1,88],[-22,86],[9,22],[-21,179],[-32,58],[-27,-44],[-10,58],[-21,34],[-12,96],[15,16],[-2,115],[-30,65],[-9,78],[-26,84],[1,27],[-16,61],[1,73],[-12,25],[3,124],[-19,-1],[8,96],[-25,49],[-11,59],[-15,12],[-7,-54],[-15,-36],[-19,-82],[-16,-24],[-15,11],[-9,-76],[-18,-24],[-7,38],[-44,91],[-10,71],[18,42],[-2,63],[-17,60],[16,84],[28,7],[6,35],[-12,64],[7,35],[-23,43],[4,127],[-13,14],[1,47],[19,2],[-1,86],[11,110],[-3,60],[15,58],[-9,72],[14,11],[8,161],[-11,26],[-29,-43],[-21,21],[-14,-20],[-12,31],[4,56],[-21,38],[-16,-50],[-10,19],[7,52],[-28,65],[-16,16],[0,102],[-22,66],[-13,-3],[-7,66],[-33,122],[-31,66],[-5,79],[-58,51],[-6,59],[-48,86],[2,33],[24,32],[-16,7],[-3,51],[-16,20],[18,55],[-13,70],[4,47],[-29,67],[-7,88],[-14,18],[-41,156],[0,1191]],[[23039,67566],[284,0],[106,0],[268,0],[241,-1],[313,1],[247,0]],[[24498,67566],[-4,-47],[10,-75],[-5,-35],[30,-60],[14,-70],[-33,-139],[0,-111],[9,-156],[12,-26],[13,-170],[28,-58],[65,-54],[16,-84],[5,-70]],[[24442,63932],[-29,38],[-2,56],[-24,53],[-2,45],[-16,11],[-13,71],[-61,-10],[-446,-29],[-275,-11],[-191,9],[-152,8]],[[26757,57673],[91,99],[7,-21],[33,75],[50,53],[19,44],[6,-40],[27,22],[155,-25],[188,-34],[0,-122],[30,73],[43,-200],[-5,-135],[83,-3],[230,-14],[88,-302],[135,-479],[93,-328]],[[28030,56336],[-48,-61],[-28,-66],[-35,-120],[-41,-182],[-17,-123],[-13,-242],[-38,-97],[-9,-90],[-34,0],[-13,30],[-24,-105],[10,-45],[-33,-64],[-7,-53],[-34,-61],[-11,40],[0,-122],[-32,-90],[-33,-18],[-35,-63],[-24,-67],[-23,-9],[-8,40],[-12,-51],[18,-70],[-9,-92],[-53,-71],[0,35],[-22,39],[-12,-37],[27,-84],[-22,-92],[-26,-28],[-18,-81],[6,-16]],[[17950,55017],[5,31],[24,-20],[25,33],[-1,40],[17,63],[1,79],[-15,136],[-42,9],[-10,59],[8,87],[0,125],[-15,37],[9,40],[-7,72],[26,34],[2,33],[28,112],[-2,137],[9,31],[-9,176],[8,23],[-9,68],[27,112],[6,99],[45,77],[34,104],[-12,101],[-14,19],[-31,98],[-14,9],[2,84],[-16,78],[-10,135],[-22,63],[-22,115],[-1,158]],[[19530,53403],[-304,2],[-260,-2],[-135,181],[-234,307],[-385,498],[-131,168],[-157,198],[6,86],[-5,57],[13,30],[12,89]],[[2683,382],[25,11],[-18,-69],[-7,58]],[[2313,265],[49,100],[16,-38],[-36,-114],[-29,52]],[[26284,65466],[377,42]],[[26661,65508],[5,-45],[28,14],[30,-70],[45,-58],[27,-83],[28,11],[10,63],[13,-55],[19,1],[-2,-35],[-65,-27],[-5,-40],[15,-40],[26,40],[18,-10],[22,55],[22,-84],[35,-46],[33,55],[30,7],[22,44],[46,51],[19,-27],[55,-6],[33,66],[52,152],[44,103],[15,5],[63,99],[29,19],[106,126]],[[27479,64235],[-30,-22],[-11,-54],[20,-119],[-5,-78],[3,-101],[-17,-102],[-21,-179],[-1,-122],[-6,-72],[-13,-7],[4,-46],[-20,-129],[10,-39],[-14,-115],[-15,-7],[-25,-72],[-31,-125],[-25,-69],[-13,0],[-21,-47],[-14,12],[-16,62],[-29,-82],[-3,-83],[-30,7],[-4,-53],[-18,-59],[1,-102],[-15,-7],[12,-79],[-3,-108],[-11,27],[-21,-84],[-10,32],[7,42],[-9,58],[-21,49],[-23,-62],[-15,-91],[0,-67],[-20,-52],[9,-101],[-1,-133],[-29,-18],[-9,-151],[-57,-52],[-18,20]],[[26230,53017],[-68,0],[-210,-9],[-318,8],[-128,-3],[3,-38],[-13,-115],[29,-142],[35,-80],[4,-51],[-16,-141],[22,-93],[-17,-28],[-24,-146]],[[25529,52179],[-39,-36],[-50,-26],[-16,14],[35,27],[4,33],[-23,99],[-19,39],[-9,86],[9,63],[-6,130],[-24,42],[-25,-204],[-1,-155],[-9,-76],[-15,6],[1,53],[-47,50],[-11,-42]],[[25685,69725],[9,49],[28,10],[-12,-82],[-12,-15],[-13,38]],[[25568,69525],[12,33],[-1,-49],[-11,16]],[[24708,71642],[4,74],[15,13],[-2,-60],[-17,-27]],[[24655,71549],[5,23],[30,31],[1,-42],[-32,-31],[-4,19]],[[24624,71578],[14,13],[7,-48],[-13,-4],[-8,39]],[[24617,71389],[16,18],[33,86],[13,-31],[-29,-33],[4,-36],[-16,-3],[-20,-38],[-1,37]],[[24564,71623],[14,19],[0,-46],[-14,27]],[[24276,71298],[20,-31],[43,17],[60,73],[17,0],[40,42],[38,77],[18,-17],[36,51],[21,64],[9,-13],[19,35],[20,-34],[11,-53],[-23,-114],[-14,-39],[8,-74],[-16,-35],[-10,-70],[9,-23],[62,95],[30,-75],[47,-40]],[[24721,71134],[7,-39],[22,14],[0,-33],[23,-9],[15,-59],[16,-141],[163,-131],[123,-99],[87,-142],[14,11],[18,-41],[17,29],[27,4],[25,-51],[11,19],[24,-48],[40,-15],[15,-26],[7,-48],[-18,-60],[18,-50],[21,18],[9,-43],[16,1],[26,-56],[8,-44],[-11,-30],[12,-38],[-5,-61],[-10,7],[11,-80],[-19,-76],[-6,-83],[36,-16],[17,47],[13,-59],[-11,-48],[-14,-145],[21,-76],[19,-16]],[[25508,69421],[-9,-57],[-1,-80],[-53,-30],[-3,-65],[-20,-79],[-23,-124],[-9,-157],[-7,-27],[20,-37],[11,6],[5,49],[16,49],[25,26],[12,96],[31,132],[35,19],[14,45],[15,-68],[-7,81],[24,156],[10,10],[13,139],[18,-17],[15,46],[3,77],[11,45],[24,-1],[0,-90],[-15,14],[-3,-165],[-21,-26],[-5,-62],[-14,-51],[4,-48],[-8,-60],[-16,-29],[-16,-73],[-20,-172],[-13,-41],[-13,-109],[-9,-158],[-8,-63],[11,-107],[-4,-58],[-35,-88],[-25,-261],[11,-137],[-2,-87],[-24,-145],[-4,-116],[-17,-81],[-13,-167],[9,-93],[-6,-43],[10,-73],[-8,-34],[12,-83],[1,-101],[7,-56],[18,-62],[-16,-171],[4,-166]],[[24498,67566],[-4,95],[-10,39],[0,110],[7,75],[-11,84],[-44,180],[-40,30],[-35,109],[-46,93],[-11,129],[-13,56],[-26,49],[-45,43],[-17,41],[-20,97],[-56,14],[-6,40],[-46,132],[-20,40],[12,104],[-3,61],[7,51],[-4,107],[-7,59],[13,33],[-7,90],[3,123],[30,125],[1,50],[-34,146],[-32,10],[4,166],[24,54],[21,152],[19,45],[24,21],[9,38],[13,-13],[9,50],[26,1],[15,67],[1,689],[22,-19],[6,79],[28,36],[21,-49]],[[17338,70470],[14,-91],[24,-107],[23,-5],[9,-45],[20,-4],[13,-33],[3,-68],[20,-89],[-25,-127],[-33,-215],[-16,-212],[-33,-136],[5,-106],[-18,-88],[-11,-84],[-30,-45],[-15,-94],[-13,-138],[-23,-78],[3,-60],[-8,-36],[15,-70],[-9,-45],[15,-45],[18,24],[16,-58],[20,15],[1,-53],[22,-43],[-14,-77],[-10,-10],[12,-75],[-12,-123],[-2,-57],[-10,-9],[-1,-59],[0,-2105]],[[16479,65813],[-288,-1],[-314,16],[-60,3],[-186,-8],[-28,7],[-56,-11],[-241,-1]],[[15306,65818],[-40,122],[-2,90],[-15,83],[-6,215],[13,161],[-6,96],[-11,21],[-6,66],[-11,1],[-13,118],[22,136],[9,94],[18,277],[-5,42],[13,42],[19,109],[15,147],[18,287],[12,346],[11,377],[0,83],[8,183],[-5,131],[12,151],[16,285],[3,157],[-3,86],[9,358],[0,76],[-12,47],[6,81],[-1,82],[-7,45],[15,36],[1,110],[-21,177],[43,-78],[-3,36],[30,29],[11,-28],[28,31],[20,51]],[[23551,59417],[0,582]],[[23551,59999],[0,901],[3,1208],[-1,407],[-22,26],[-13,-16],[-25,47],[-6,68],[-15,37],[-6,98],[-31,89],[-19,85],[-2,47],[14,7],[9,99],[16,32],[4,62],[14,-25],[12,45],[-18,31],[14,20],[-19,58],[4,34],[-25,13],[-5,-37],[-47,50],[-29,107]],[[28814,58761],[10,1],[18,-62],[3,-79],[-17,17],[4,38],[-18,85]],[[28806,57893],[44,78],[20,15],[8,139],[7,271],[-16,174],[12,-85],[7,-123],[-7,-224],[-12,-209],[-14,17],[-49,-53]],[[28646,57495],[68,236],[-3,-31],[-65,-205]],[[28584,57233],[6,-14],[16,117],[38,146],[-42,-193],[-13,-92],[-5,36]],[[28432,57261],[27,29],[74,46],[-101,-75]],[[27156,59520],[234,-33],[37,3],[114,-21],[369,-2],[202,3],[138,1],[233,-2],[0,10],[292,-2]],[[28775,59477],[13,-225],[28,-284],[17,-108],[35,-272],[-8,14],[-15,148],[-19,81],[-12,9],[-5,164],[-8,85],[-10,37],[-11,150],[1,49],[-15,72],[-25,14],[-4,50],[-11,-31],[6,-60],[27,-106],[35,-344],[-19,65],[1,37],[-17,98],[-11,12],[2,-66],[13,-39],[-30,26],[-31,115],[-18,14],[31,-114],[6,-58],[-30,-30],[-23,71],[10,-103],[-44,49],[19,-51],[-30,-16],[-13,-66],[-34,-16],[-7,51],[-20,-12],[-12,121],[1,62],[13,82],[-22,-70],[-1,-74],[20,-184],[-13,-54],[49,-4],[42,49],[9,-55],[55,70],[30,-5],[14,-80],[-14,-80],[13,-232],[10,138],[-1,136],[18,54],[-8,28],[22,22],[24,-53],[14,-111],[-1,-103],[6,-65],[-18,-10],[13,-65],[-29,-68],[-9,39],[-33,-139],[-9,-85],[-10,26],[-4,-74],[-24,-38],[-31,3],[-16,54],[-12,-30],[-7,82],[-21,-67],[-20,43],[-13,114],[27,-5],[10,56],[-23,-28],[-17,13],[-11,-31],[18,-145],[-24,40],[-28,-4],[-61,75],[-18,62],[3,-51],[19,-69],[11,17],[29,-57],[47,-69],[47,-31],[-3,-112],[-17,19],[-22,-69],[26,-34],[-8,-54],[-39,-110],[-31,-39],[-49,123],[-18,75],[35,-195],[49,-67],[36,85],[37,-4],[10,106],[7,-22],[4,-124],[11,32],[16,-21],[-11,89],[32,-42],[-13,-90],[-32,-106],[-28,-131],[-18,2],[-11,77],[-1,-97],[-32,19],[-44,5],[-40,-50],[-22,6],[-25,-63],[-29,-104],[-51,-102],[-55,-158],[-24,-101],[-27,-175],[-22,-218],[-2,-62],[-11,44],[-31,33],[-36,8],[-39,-14],[-45,-60]],[[21482,59992],[232,4],[281,6],[230,-2],[232,-1],[253,0],[250,0],[293,0],[298,0]],[[23588,56087],[-30,55],[-34,2],[-5,42],[-40,34],[-22,74],[-4,52],[-22,2],[-3,46],[-43,70],[-11,-79],[-31,-32],[-51,16],[-2,55],[-56,-41],[-4,-55],[-21,-7],[-23,53],[-4,-27],[-30,-29],[-28,1],[-5,-79],[-17,-25],[-19,20],[-7,-82],[-9,-9],[-19,103],[-22,-4],[-7,57],[-29,36],[13,43],[-25,27],[-5,-73],[-19,-29],[-10,51],[-24,-1],[-11,107],[-21,-14],[3,-71],[-30,-102],[-1,-83],[-15,-7],[-15,95],[11,43],[-13,88],[-18,-67],[-9,35],[-18,-80],[-25,26],[1,72],[-26,-7],[-10,63],[-23,46],[-54,-165],[-33,59],[11,103],[-39,16],[-10,81],[8,58],[-9,38],[-13,-47],[-54,50],[-16,-87],[-18,-23],[-32,114],[-25,-32],[-21,4],[-43,55],[-17,39],[-17,-22],[-41,18],[-6,144],[-23,90],[-24,49],[-5,-90],[-10,-11],[-18,46],[-23,8],[-5,-49],[-27,4],[-53,199],[-17,35],[-15,-22],[0,2258],[-293,-1],[-244,1],[-299,0]],[[28836,61125],[6,32],[107,38]],[[28949,61195],[-30,-174],[-17,23],[-22,-75],[-20,-112],[-15,-148],[4,-44],[-23,-126],[6,0],[-20,-138],[-13,-55],[-10,-121],[-23,-88],[-19,0],[-16,162],[3,89],[13,77],[-4,45],[11,146],[32,191],[3,72],[27,4],[13,67],[-11,102],[19,-2],[-1,35]],[[28725,61110],[15,0]],[[28740,61110],[-15,0]],[[27075,60626],[12,-28],[-19,-62],[12,-17],[6,-99],[21,-85],[26,-14],[23,-87],[33,6],[40,78],[15,75],[38,-120],[31,51],[59,42],[15,38],[-10,43],[7,53],[25,-67],[61,118],[11,9],[10,-68],[49,99],[-8,66],[30,74],[-21,74],[37,213],[45,137],[21,134],[3,94],[35,92],[-6,44],[19,57],[15,90],[-2,65],[13,121],[31,-47],[17,-109],[46,-53],[28,95],[22,186],[11,7],[6,103],[22,119],[34,-101],[23,142],[20,59],[0,-36],[27,90],[5,-20],[14,62],[-6,16],[47,158],[-10,35],[6,69],[22,108],[-8,11],[6,124],[144,-388],[23,189],[8,31]],[[28259,62702],[42,-18],[7,-46],[24,-48],[-20,-92],[18,-82],[34,-15],[27,-50],[0,-44],[27,-21],[8,-35]],[[28426,62251],[24,-97],[-2,-69]],[[28448,62085],[0,-76],[-26,-61],[1,-45]],[[28423,61903],[-20,-20],[-10,49],[-5,-115],[-20,-137],[13,-121],[11,-10],[44,43],[12,38],[8,-30],[-5,-73],[19,-65],[0,-49],[34,-59],[22,4],[9,-41],[11,33],[43,-101],[17,-103],[36,-43],[30,-67],[-8,-85],[-12,6],[1,-140],[-10,-54],[17,-44],[-34,11],[-20,45],[-10,-28],[-21,151],[-39,75],[-35,160],[-18,0],[33,-91],[17,-113],[33,-66],[8,-88],[17,-64],[27,0],[4,-35],[32,-22],[-17,-32],[29,-95],[3,-86],[-8,-76],[-39,111],[0,-81],[15,-83],[-41,-33],[10,-28],[24,12],[8,-67],[24,-94],[-9,-101],[-10,17],[-20,-61],[-15,75],[-45,114],[6,37],[-12,74],[-30,-34],[-10,46],[-22,37],[-21,-41],[12,-33],[29,8],[24,-68],[8,57],[11,-187],[16,-9],[28,-84],[-1,-88],[38,49],[9,-43],[-1,90],[66,-64],[26,16],[13,-187],[23,-245]],[[27769,63167],[-3,-600],[18,26],[22,77],[15,6],[10,68],[11,7],[37,125],[35,-36],[43,218],[41,-108],[52,-21],[4,67],[16,19],[13,58],[24,-24],[23,88],[21,-15],[26,-91],[49,2],[-12,-49],[10,-76],[14,-10],[0,-69],[11,-9],[0,-104],[10,-14]],[[24271,51361],[29,66],[24,-8],[11,-46],[26,-22],[-16,-92],[-22,-13],[-52,115]],[[24970,52063],[-28,-28],[-27,-103],[-3,-54],[-24,27],[-11,-48],[5,-68],[31,14],[-5,-58],[11,-39],[31,6],[4,129],[25,55],[2,48],[31,-49],[-24,-45],[18,-40],[5,-67],[16,18],[13,84],[8,-73],[-23,-49],[24,-4],[-24,-72],[-16,19],[22,-95],[-25,37],[-8,-108],[-28,34],[-2,-62],[-43,-56],[12,-58],[-12,-14],[13,-70],[19,-11],[16,-130],[12,23],[42,-15],[23,-99],[10,47],[24,-106],[5,-76],[20,32],[6,-41],[-30,-24],[20,-28],[-8,-52],[-10,30],[-12,-53],[1,-92],[-21,35],[-9,71],[-43,-176],[3,64],[21,82],[11,93],[-4,72],[-16,-81],[-13,-7],[-24,107],[-34,45],[-9,42],[-57,33],[5,43],[18,-6],[42,43],[-44,2],[-55,86],[-20,-152],[1,-59],[-21,-19],[19,-70],[-51,-137],[-10,131],[-26,123],[-12,-56],[-12,107],[-11,-67],[-27,25],[-10,-115],[-31,-91],[-36,-72],[-5,51],[-25,79],[-29,-2],[-18,42],[-45,41],[-14,63],[21,59],[18,-65],[8,24],[0,-102],[15,24],[-4,96],[-35,113],[-21,-54],[-7,141],[-29,-50],[-18,89],[-8,104],[-23,7],[3,116],[-41,5],[-23,-46],[-6,59],[13,85],[-14,7],[-56,-89],[-34,-7],[26,-157],[21,0],[-2,-39],[-29,1],[-49,-63],[-83,65],[-33,40],[-63,110],[-59,63],[-82,-1],[-75,-39],[-27,-53]],[[23768,51492],[-24,149],[19,53],[19,147],[21,88],[5,100],[-7,66],[4,95],[-11,16],[-3,102],[15,46],[-7,166],[13,24],[-1,54],[15,46],[13,143],[3,95],[11,86],[-13,48],[20,38],[-12,54],[4,124],[-14,-23],[-8,111],[-19,47],[5,74],[-7,88],[-16,33],[10,53],[-21,15],[-14,83],[12,120],[-8,85],[-29,159],[-13,12],[-19,90],[0,1187]],[[25021,51921],[43,114],[-16,-66],[19,-46],[-15,-70],[-3,61],[-28,7]],[[26345,70257],[22,-1],[43,-48],[-18,-58],[-23,19],[-24,88]],[[26112,70220],[21,26],[-7,-40],[-14,14]],[[26082,70243],[1,32],[21,-38],[-6,-26],[-16,32]],[[26055,70012],[18,184],[18,-39],[2,-138],[-19,-42],[-19,35]],[[26035,70171],[13,2],[-7,-51],[-6,49]],[[25984,69829],[15,-28],[0,-42],[-15,70]],[[25934,69474],[5,23],[19,-25],[-6,-98],[-17,55],[-1,45]],[[25908,69328],[20,29],[-8,-47],[-12,18]],[[25754,71110],[16,18],[7,-30],[-5,-55],[6,-36],[-18,-1],[-6,104]],[[25722,65541],[39,93],[18,60],[38,263],[36,158],[16,122],[14,154],[11,254],[-1,175],[-4,150],[-8,111],[-47,330],[-16,172],[-16,119],[2,71],[19,80],[8,126],[-8,135],[-15,101],[29,94],[19,129],[21,111],[12,291],[-9,80],[7,49],[40,42],[6,91],[-1,75],[8,35],[14,-17],[17,80],[17,-35],[17,13],[19,123],[17,43],[17,110],[22,28],[2,-39],[-23,-58],[14,-80],[-19,-71],[9,-9],[-14,-185],[3,-65],[13,-8],[19,149],[1,90],[13,21],[-7,-158],[-16,-44],[-5,-68],[13,-14],[37,213],[9,210],[-7,93],[5,93],[20,59],[45,60],[26,-14],[36,37],[-2,33],[-32,16],[-16,54],[-7,102],[12,70],[17,26],[20,67],[-23,69],[52,-28],[33,27],[81,-143],[5,29],[23,-6],[32,-35],[22,-72],[7,-75],[56,-26],[19,-59],[20,-33],[15,9],[33,-73],[17,7],[20,-32],[23,-71],[-9,-16],[15,-108],[11,-34],[15,-129],[-20,22],[-10,40],[-22,-59],[3,-85],[14,-57],[22,-27],[6,-167],[5,-27],[-11,-138],[2,-61],[-7,-235],[-30,-89],[-19,11],[-11,-54],[-9,-207],[-28,-15],[0,-58],[-43,2],[-21,-85],[-2,-122],[-10,-58],[11,-97],[17,-37],[52,-64],[40,147],[9,-11],[20,119],[-13,-8],[34,81],[1,37],[21,83],[31,14],[30,33],[5,35],[27,27],[32,-43],[21,-50],[17,-123],[10,-36],[8,-85],[8,-274],[14,-129],[-1,-93],[15,-292],[19,-113],[-11,-92],[-7,-137],[4,-55],[-16,-181],[-18,-64],[-28,10],[9,38],[-15,6],[24,79],[-21,28],[-33,-73],[10,-64],[-14,-4],[-11,-52],[1,-81],[-15,-119],[-39,-41],[-18,-91],[-2,-175],[-16,-27],[6,-50],[-22,-57],[-1,-33],[-19,-14],[-33,-144],[-4,-89]],[[25043,72652],[23,76],[52,78],[60,76],[31,79],[33,46],[6,-11],[29,30],[1,-24],[-41,-90],[-3,-36],[-26,-59],[-51,-54],[-42,-66],[1,-21],[25,1],[-70,-79],[-28,54]],[[24721,71134],[71,93],[37,33],[51,142],[16,26],[32,17],[12,-17],[33,27],[27,3],[52,80],[31,95],[37,3],[10,20],[17,100],[35,66],[30,80],[22,22],[21,57],[21,87],[57,94],[48,30],[69,-2],[24,-39],[1,-44],[-29,-20],[-39,3],[5,-52],[-33,-52],[-31,-96],[-20,-34],[-3,-58],[-28,-70],[-5,-66],[-21,-55],[-11,-170],[9,-73],[19,87],[38,106],[32,36],[-31,-87],[45,41],[23,-18],[28,4],[44,-51],[4,-43],[14,11],[22,-73],[4,-51],[17,-47],[5,-50],[30,-73],[0,-60],[24,-48],[49,2],[32,40],[12,-2],[13,-72],[15,-32],[16,1],[18,49],[32,-79],[24,97],[23,66],[33,30],[59,111],[14,-21],[59,42],[81,-19],[29,6],[63,85],[27,14],[21,-8],[36,11],[-20,-82],[0,-180],[-8,-33],[11,-40],[23,9],[26,-48],[20,8],[25,40],[13,-3],[10,-81],[15,7],[34,93],[46,-1],[8,39],[27,-5],[5,-30],[-10,-100],[11,-112],[-4,-109],[-25,4],[-17,-35],[21,-41],[23,9],[24,-46],[-12,-48],[26,-86],[12,12],[11,-50],[-5,-25],[-29,-7],[-28,28],[-22,-13],[-21,19],[4,-36],[-46,1],[9,31],[-15,25],[-5,-44],[-12,34],[-7,-33],[-4,62],[-31,35],[-9,-21],[0,-74],[-14,-34],[11,-104],[-14,-15],[-25,67],[-21,33],[-25,93],[-52,49],[-43,53],[-49,-2],[-36,-143],[-44,13],[-23,-15],[-6,-55],[-42,52],[-37,-2],[-22,-22],[-14,-44],[0,-59],[-13,-79],[-45,-48],[-1,-44],[-12,4],[-10,-111],[-26,37],[-2,41],[14,22],[12,72],[27,104],[-15,49],[-19,-72],[-37,31],[1,-71],[-25,-104],[-21,-9],[-6,-35],[-11,185],[16,53],[-9,33],[-22,-108],[6,-80],[-38,-98],[-22,-107],[-39,-231],[-41,-204],[-17,-50],[2,-66]],[[26542,70447],[10,59],[12,-48],[10,48],[26,33],[-13,54],[39,-13],[13,-97],[17,-26],[-16,-77],[-15,-3],[-21,35],[-16,-18],[-11,29],[-8,-26],[-27,50]],[[30344,64988],[43,16],[17,81],[17,-105],[-5,-37],[-40,-5],[-32,50]],[[30185,65189],[27,71],[9,-8],[-36,-63]],[[30176,65060],[19,7],[19,94],[27,56],[12,-15],[4,-62],[16,-38],[11,43],[1,-78],[-80,-20],[-12,-41],[-17,54]],[[29724,66666],[325,-35],[13,53],[18,-6],[-1,62],[15,36],[19,-18],[9,62],[28,29],[32,-14]],[[30182,66835],[11,-211],[24,-41],[17,43],[10,-67],[-17,-62],[-6,30],[-17,-36],[-34,-37],[-8,-42],[11,-18],[-26,-43],[-14,-58],[5,-56],[-19,-42],[14,-66],[21,8],[42,-21],[35,-193],[-8,-89],[-12,-6],[16,-64],[31,-30],[5,-79],[-4,-50],[19,-63],[50,-32],[9,-23],[12,33],[54,52],[6,116],[-12,50],[-7,-52],[-7,172],[-17,34],[-15,-37],[-10,39],[16,21],[20,-17],[23,-58],[13,-79],[12,-122],[3,-193],[-9,-78],[-9,64],[-55,-22],[-20,-45],[-20,31],[-27,-36],[-16,-62],[-54,-36],[13,72],[-6,114],[10,21],[-26,43],[-14,-105],[-11,-24],[-32,-12],[-7,-41],[6,-46],[-16,-36],[-38,-13]],[[30097,65235],[-3,189],[-18,17]],[[30076,65441],[-8,42]],[[30068,65483],[-10,48],[-19,33],[-5,50],[2,87],[-12,-6],[1,146],[-117,-12]],[[29908,65829],[0,18],[-94,8],[-172,7],[-17,-45],[1,45],[-188,15]],[[29438,65877],[-6,43],[68,768]],[[17306,73966],[185,3],[89,-1]],[[31706,38292],[28,-53],[-11,-32],[-17,85]],[[31642,38010],[39,60],[45,-28],[-43,-48],[-41,16]],[[31170,38311],[32,68],[5,97],[120,-41],[40,21],[27,-25],[19,27],[105,-56],[8,20],[57,-50],[43,-83],[12,-138],[-49,-92],[-18,-151],[-30,-56],[-22,10],[-58,-79],[-25,77],[-20,-48],[-22,62],[-31,-34],[-42,35],[-64,-70],[-7,41],[-64,-16],[8,246],[8,40],[-32,195]],[[30982,37983],[23,44],[-10,-80],[-13,36]],[[27310,45692],[54,76],[-42,-94],[-12,18]],[[27118,45524],[26,51],[-9,55],[86,185],[20,-40],[21,-144],[-27,-39],[-31,38],[-2,-33],[-48,-78],[-36,5]],[[27004,47898],[13,-117],[25,-55],[-3,-35],[-21,50],[-14,157]],[[26203,51417],[20,-34],[44,86],[18,15],[-67,-112],[-15,45]],[[26168,51479],[42,-6],[-19,-47],[-23,53]],[[27221,52678],[5,-10],[-4,-235],[7,-9],[7,-217],[23,-390],[9,-77],[6,-148],[25,-257],[55,-474],[72,-456],[37,-199],[14,-147],[-18,-60],[-5,-130],[10,-215],[18,-164],[35,-268],[34,-403],[27,-234],[19,-203],[18,-259],[-2,-225],[-20,-597],[-6,-379],[-7,-48],[-15,26],[-34,-253],[1,-153],[-26,-215],[19,-5],[-2,83],[30,72],[-41,-273],[-9,2],[-9,121],[-32,0],[-21,-55],[-59,13],[-10,-42],[-51,-27],[-24,123],[5,120],[9,16],[-13,152],[-34,254],[-25,102],[-74,142],[-12,-52],[-32,278],[-8,236],[-10,98],[-41,175],[-15,8],[-10,-60],[-11,186],[-10,44],[15,21],[8,-63],[12,182],[-11,104],[11,56],[-17,28],[-18,-49],[10,-154],[-29,-31],[-55,347],[-21,195],[-62,362],[16,-49],[30,119],[27,167],[13,25],[12,85],[-19,117],[-8,-21],[0,-113],[-18,29],[7,35],[-7,104],[-31,71],[-15,-107],[20,-30],[17,-106],[-15,-134],[-20,1],[2,-93],[-9,-16],[0,111],[-26,134],[-6,75],[9,189],[4,-96],[7,188],[-7,56],[13,61],[16,215],[8,42],[6,156],[-15,153],[3,87],[-13,41],[14,71],[-2,53],[-30,143],[-8,118],[-46,22],[-15,-55],[-10,162],[-23,28],[-13,152],[-51,112],[-2,155],[-38,91],[-12,105],[-26,120],[-68,131],[-28,80],[-37,-35],[-32,23],[1,-29],[-27,-69],[7,-44],[-26,23],[-1,-35],[28,-21],[-2,-50],[-30,35],[-23,-22],[-35,-91],[-12,-9],[-50,-106],[-9,58],[-17,-72],[-40,-6],[-31,-45],[-23,17],[4,124],[-29,163],[-29,39],[-25,91],[-29,56],[-21,67],[-48,108],[-39,64],[-77,82],[-72,26],[-32,-4],[-60,-33],[-56,-43],[-44,-8],[-57,-47]],[[29991,69327],[1,65],[19,64],[7,106],[34,101],[15,-61],[27,-5],[13,70]],[[30107,69667],[15,-757],[6,-411],[6,-539],[4,-313],[7,-22],[-10,-159],[8,-93],[39,-113],[-3,-129],[34,-78]],[[30213,67053],[-25,-139],[-6,-79]],[[28861,63061],[1,-28]],[[28862,63033],[-1,28]],[[28901,63261],[-27,-135]],[[28874,63126],[-21,-41],[-7,-62],[14,-41],[-9,-97],[28,-154],[15,-39],[12,-141],[-5,-45],[5,-111],[15,-44],[11,-124],[31,-124],[31,-40],[9,-374]],[[29003,61689],[-96,1],[-84,9],[-26,1395],[0,74]],[[23410,50792],[69,234],[42,49],[-111,-283]],[[22852,49339],[20,72],[-2,-66],[-18,-6]],[[22782,47846],[30,5],[14,-224],[-19,209],[-25,10]],[[22776,48207],[5,440],[5,131],[34,320],[12,59],[20,167],[23,8],[-43,-247],[-34,-294],[-10,-150],[-7,-226],[5,-211],[24,-349],[-15,94],[-19,258]],[[23768,51492],[-45,-11],[-27,-24],[-110,-174],[-39,-69],[-41,-105],[1,44],[31,91],[28,62],[19,-20],[-14,68],[-61,-56],[18,134],[1,124],[-16,41],[-17,-26],[-16,-101],[-20,30],[-20,-76],[9,-36],[-9,-71],[29,-51],[-12,-28],[17,-45],[0,-140],[-22,-39],[-36,-123],[-15,24],[-3,-86],[12,-58],[-73,-234],[-28,-39],[-80,-174],[-87,-154],[-52,-108],[-33,-93],[-15,-75],[-32,-62],[-54,-143],[-27,-92],[-58,-248],[19,116],[18,162],[32,44],[-4,48],[34,38],[28,108],[45,59],[-3,88],[-48,-120],[-17,-2],[-7,108],[-41,2],[18,-60],[-5,-78],[7,-54],[-17,-74],[-34,-66],[16,164],[-16,-52],[-2,-101],[-11,82],[-37,-72],[-21,-75],[23,-58],[8,48],[24,52],[0,-94],[-17,-72],[-29,-171],[-16,61],[-60,-1],[-14,-28],[14,-32],[22,19],[7,-122],[32,-49],[-22,-144],[-13,-185],[-12,-103],[-23,-13],[-1,72],[-30,-100],[-28,115],[21,-167],[29,-15],[29,38],[-10,-482],[-5,-48],[4,-131],[16,-226],[7,-22],[26,-248],[-8,-51],[10,-153],[36,-33],[1,-129],[-8,14],[-41,-54],[-14,-92],[-41,54],[-37,151],[-31,13],[-31,44],[-25,-15],[-31,19],[-19,-22],[-29,26],[-37,99],[-21,80],[-59,13],[-22,109],[-20,47],[-21,-20],[-22,55],[-30,-5],[-3,94],[-21,68],[-11,220],[-17,138],[-33,117],[3,35],[-19,58],[5,178],[-4,85],[-27,79],[9,26],[7,162],[-13,23],[4,76],[-23,87],[-30,19],[-31,138],[-18,31],[-15,210],[-16,14],[-8,84],[-18,92],[-36,61],[-23,102],[2,38],[-17,100],[-14,214],[-27,85],[-11,170],[-32,159],[-7,183],[-28,86],[-6,86],[-57,137],[-16,109],[-26,21],[-6,44],[-24,26],[-2,109],[-27,26],[-3,57],[-21,94],[-25,-24],[-47,0],[-39,54],[-56,-26],[-18,8],[-52,99],[-11,-10],[-13,-124],[-36,25],[-9,-46],[-33,2],[-7,-81],[-32,-180],[-9,-190],[-11,-5],[-7,-108],[10,-45],[-23,-58],[-13,3],[-11,-90],[-17,-44],[-5,-88],[-11,-16],[-64,58],[-46,157],[-47,29],[-18,98],[-66,54],[-41,99],[-11,98],[-36,45],[-48,144],[-11,113],[-37,210],[-7,151],[6,149],[-21,142],[-28,105],[-7,178],[-9,63],[-14,12],[-11,91],[-15,3],[-16,71],[-27,65],[-12,-13],[-83,226],[-14,112],[-46,95],[-27,142],[-18,29],[-5,60],[-34,38],[-41,98],[-44,291],[-42,60]],[[22823,73968],[230,-1],[120,0],[228,-1],[1,449],[26,-37],[28,20],[29,-54],[8,-34],[14,-202],[7,-25],[0,-116],[9,0],[9,-135],[-3,-118],[30,-75],[42,-27],[6,19],[43,-17],[5,-50],[66,-12],[45,-16],[14,-132],[42,1],[49,33],[1,54],[33,41],[39,18],[8,-22],[54,0],[71,-98],[26,4],[0,-50],[-18,-6],[-3,-37],[15,-30],[42,13],[14,-55],[-4,-57],[28,-153],[28,32],[-10,80],[12,44],[16,-11],[42,16],[15,-44],[-2,-65],[14,-38],[18,6],[8,-36],[42,-9],[4,-67],[16,-52],[23,13],[-2,-75],[22,28],[16,-22],[16,24],[34,17],[47,113],[55,75],[13,-7],[1,-77],[20,-87],[34,6],[16,23],[7,-32],[118,18],[31,-31],[8,-66],[27,-47],[36,41],[19,-20],[23,6],[-18,-77],[-31,-62],[-38,-39],[-12,-32],[-117,-104],[-40,-44],[-54,-88],[-37,-79],[-74,-187],[-31,-110],[-63,-202],[-30,-48],[-58,-171],[-17,-21],[-70,-160],[22,-94]],[[23009,70400],[-4,107],[6,66],[-7,78],[-6,210],[-18,50],[-18,126],[-4,103],[-15,104],[5,73],[-4,63],[9,183],[-17,42],[2,226],[-7,149],[3,53],[-10,164],[6,44],[-3,109],[-55,391],[-6,124],[-19,127],[0,98],[8,88],[-13,85],[10,33],[-10,66],[5,66],[-4,53],[18,121],[-12,46],[-6,99],[-9,6],[3,71],[-11,52],[-3,92]],[[31859,38270],[27,28],[0,-65],[-27,37]],[[31829,37513],[3,72],[41,48],[11,-43],[35,18],[-45,-78],[-45,-17]],[[29908,65829],[4,-410],[-3,-278],[-13,-13],[4,-74],[-9,-26]],[[29891,65028],[-5,19],[-29,-7],[-42,-36],[-12,27],[-7,-40],[-37,-6],[-14,-25],[-19,20],[-25,-35],[-21,27],[-26,-32],[-32,22],[-22,-17],[-1,55],[-30,-107],[-8,7],[-24,-74],[-11,15],[-65,-85],[-50,-99],[-21,-19]],[[29390,64638],[1,31],[-20,104],[68,130],[-19,96],[8,378],[10,500]],[[28861,63061],[13,65]],[[29101,65072],[110,-215],[111,-205],[-7,-125],[-24,-155],[-3,-55]],[[29288,64317],[-19,-70],[-18,-5]],[[29251,64242],[-1,1]],[[29250,64243],[-11,-17],[-4,-77],[-11,-19],[-2,-97],[19,-28],[17,18],[35,-53],[7,44],[1,-174],[-14,-205],[-13,-247],[-6,-166],[-40,-248],[-17,-64],[-8,-73],[-26,-96],[-19,-29],[-34,-99],[-28,-152],[2,-13],[-44,-190],[-30,0],[19,172],[5,83],[-40,71],[-23,-9],[-7,-36],[-9,62],[-33,66],[-13,63],[-8,-5],[-28,114],[-20,25],[7,136],[-12,33]],[[20923,73967],[193,0],[141,0],[231,-1],[145,1],[294,0],[174,-1],[229,1],[221,1],[272,0]],[[28725,61110],[1,91],[17,-32],[-3,-59]],[[28723,61274],[11,40],[0,-66],[-11,26]],[[28713,61354],[11,47],[8,-35],[-19,-12]],[[29003,61689],[-41,-413],[-13,-81]],[[28836,61125],[-21,20],[-27,-19],[-18,-65],[-4,73],[35,119],[-28,-19],[10,105],[-34,-16],[5,59],[22,16],[11,60],[-27,14],[20,117],[-25,-68],[-1,-70],[-21,73],[14,15],[-11,60],[-15,-80],[9,-28],[-8,-63],[-6,47],[-29,107],[-14,7],[3,-60],[-17,88],[-13,126],[18,0],[12,49],[-22,41],[17,61],[18,0],[17,-52],[24,-18],[-23,62],[-21,87],[-31,63],[-13,-82],[0,82],[22,119],[14,-76],[5,127],[-16,81],[-24,-62],[-6,-72],[0,104],[16,101],[8,-51],[20,-11],[11,31],[-6,55],[11,54],[-18,-27],[0,-71],[-9,142],[-9,18],[15,130],[15,43],[16,90],[24,-12],[-2,48],[14,37],[-4,48],[13,136],[-18,-63],[-23,-2],[-7,-56],[17,-46],[-46,-116],[-4,130],[-9,-99],[4,-45],[-11,-42],[-4,101],[-5,-86],[-14,-19],[-6,-71],[-26,-41],[-22,73],[22,-128],[18,-26],[-2,-93],[12,-47],[-16,-40],[-2,-82],[-22,-69],[11,-39],[-17,-55],[9,-50],[3,-227],[7,-54],[30,-112],[-21,-108],[23,6],[-3,-95],[19,-92],[-1,-103],[-28,65],[-4,64],[-11,-66],[-15,35],[-17,93],[-39,37],[-6,-28],[-15,35],[-6,111],[-12,-18],[8,-90],[-23,46],[-15,56],[-10,120],[-54,-99],[-11,26],[-7,116],[10,81],[30,97]],[[28448,62085],[36,118],[-36,119],[-22,-71]],[[30827,68327],[14,44],[19,-42],[-16,-46],[-5,51],[-12,-7]],[[30819,68536],[8,56],[6,-73],[-14,17]],[[30780,68236],[19,12],[-3,-77],[-14,-4],[-2,69]],[[30762,68405],[16,69],[31,-87],[-28,-82],[-15,42],[-4,58]],[[30713,68260],[29,23],[10,-44],[-12,-54],[-17,8],[-10,67]],[[30704,68478],[11,110],[12,-16],[-15,-140],[-8,46]],[[30704,68279],[7,42],[26,43],[5,-44],[-38,-41]],[[30107,69667],[37,39],[19,-121],[20,78],[3,73],[-7,41],[13,36],[36,-63],[8,34],[-16,54],[-12,73],[22,107],[22,65],[51,84],[-10,70],[21,66],[23,45],[0,80],[-15,3],[8,39],[-9,28],[18,93],[4,59],[-15,48],[24,163],[31,91],[11,7],[16,326],[120,500],[96,389],[44,-31],[9,-28],[-6,-36],[3,-154],[40,-79],[51,74],[29,4],[9,50],[29,12],[26,-14],[1,67],[27,20],[36,-45],[4,-30],[52,-116],[18,-101],[27,-49],[1,-542],[1,-375],[1,-392],[8,-29],[-15,-41],[14,-69],[-13,-23],[-1,-147],[24,-11],[12,-43],[44,-50],[17,19],[6,-88],[-19,-46],[9,-77],[12,-38],[0,-54],[-17,-73],[24,-141],[18,-40],[14,73],[36,-32],[19,-108],[16,-150],[14,-31],[2,-122],[-27,-77],[-33,-112],[-23,21],[-4,-46],[-3,95],[-23,-31],[5,-45],[-12,-54],[-24,40],[-14,-2],[-6,-110],[-37,2],[-13,-43],[-10,46],[-19,31],[-1,-91],[-13,-106],[-7,81],[-4,-62],[-6,51],[-19,33],[19,-97],[-12,14],[-13,-94],[-15,57],[-5,116],[-14,-10],[-11,58],[-5,-66],[-15,-14],[22,-69],[8,-74],[-14,-45],[-27,-2],[9,-49],[-21,-23],[-12,35],[-6,65],[17,126],[-17,-27],[1,117],[-11,-9],[-10,-88],[-18,-23],[6,-46],[-5,-55],[11,-93],[-60,122],[-25,-24],[2,110],[10,47],[0,76],[-7,-54],[-21,-44],[-10,33],[-20,-40],[12,-77],[-1,-37],[-33,-163],[-7,-118],[15,11],[-33,-109],[-15,-79],[-62,52],[-7,-90],[-11,-70],[-13,49],[-1,-53],[-24,15],[-5,-77],[-8,33],[-2,125],[-4,-113],[-13,-89],[-28,-45],[-4,84],[-18,8],[-26,-64],[-5,22],[26,134],[-19,-5],[-33,-75],[-32,-139],[13,-60],[3,-68],[-41,-28],[-12,-52],[4,-48],[12,4],[-28,-80],[3,-33],[-28,-20],[-12,-39],[-9,-71],[5,-32],[-27,-177],[-9,-12]],[[31088,68742],[15,-17],[-9,-26],[-6,43]],[[31072,68739],[17,-42],[-8,-29],[-9,71]],[[6432,39899],[5,62],[30,77],[29,144],[3,63],[-17,92],[-6,105],[18,83],[70,-173],[42,-31],[44,-85],[28,-82],[28,-118],[-3,-132],[24,2],[7,-115],[47,-129],[-3,-56],[-42,-150],[-61,-109],[-29,9],[-58,-150],[-42,-247],[-63,129],[-11,94],[9,269],[-18,164],[-8,142],[-23,142]],[[6255,41279],[9,106],[21,28],[31,-155],[44,57],[26,-37],[33,-105],[28,-33],[7,-87],[-18,-81],[-40,-28],[-51,-62],[-18,34],[-6,203],[-44,34],[-22,126]],[[6254,40827],[37,88],[4,-100],[-31,-19],[-10,31]],[[6154,41263],[20,31],[30,-28],[21,-97],[-7,-64],[-37,-40],[-12,137],[-15,61]],[[6085,41503],[13,129],[88,-54],[66,-17],[-23,-100],[-23,-31],[-62,67],[-44,-18],[-15,24]],[[5815,42048],[43,8],[32,136],[11,12],[37,-214],[-2,-78],[18,-56],[4,58],[31,-189],[-45,-50],[-20,59],[-63,-6],[-8,87],[-25,124],[-13,109]],[[5394,42576],[16,128],[42,97],[64,-10],[16,-82],[-11,-109],[1,-106],[-32,-106],[-44,27],[-19,72],[-25,30],[-8,59]],[[5266,42343],[6,70],[48,119],[-6,-114],[-23,-34],[-13,-100],[-12,59]],[[90203,32586],[42,39],[25,202],[27,-64],[-8,-90],[-34,-111],[-20,-203],[-20,22],[1,149],[-13,56]],[[90472,34413],[17,97],[4,-176],[-21,79]],[[90343,33370],[34,92],[11,-46],[-45,-46]],[[90507,37942],[13,125],[14,1],[-27,-126]],[[90502,34533],[10,113],[24,85],[-17,-227],[-17,29]],[[30097,65235],[-20,-48],[-6,197],[-7,-8],[-2,-145],[-23,16],[15,130],[22,64]],[[30019,65192],[5,112],[7,-1],[0,-92],[-12,-19]],[[29960,64842],[10,59],[8,-66],[-18,7]],[[30068,65483],[-9,-79],[-14,92],[-23,69],[7,-81],[-12,-56],[-11,-104],[8,-118],[-18,-118],[-37,-10],[-28,-37],[-39,-29],[-1,16]],[[16868,55901],[17,-5],[45,-75],[20,-87],[-4,-42],[-32,23],[-13,37],[-1,81],[-21,17],[-11,51]],[[16866,55382],[9,2],[25,-129],[38,-119],[-22,-23],[-21,60],[-29,209]],[[16597,55668],[13,8],[27,-63],[-31,2],[-9,53]],[[16499,56578],[47,-2],[21,-45],[18,-4],[16,45],[12,-24],[-11,-45],[-43,-40],[-29,-2],[-16,24],[0,60],[-15,33]],[[16410,56511],[57,39],[-1,-42],[19,-22],[3,-44],[-43,-55],[-35,124]],[[16354,56548],[24,45],[16,-51],[-15,-16],[-25,22]],[[17950,55017],[-207,-60],[-162,-51],[-138,-46],[-163,-58],[-12,160],[-21,-8],[-5,161],[3,83],[-8,160],[-13,128],[-50,246],[-25,62],[-24,88],[-9,-3],[-20,95],[-40,77],[-44,144],[-26,38],[-1,-47],[-22,-21],[-46,81],[11,70],[-17,140],[-26,97],[-53,-7],[-20,-38],[-11,36],[-26,15],[-78,119],[-20,150],[-28,50],[-20,65],[-28,48],[-30,1],[-11,-24],[-23,27],[-24,-13],[-36,62],[-77,9],[-52,-24],[-11,88],[-37,63],[12,154],[-10,54],[7,101],[-16,68],[11,187],[-2,72],[-18,55],[-15,-12],[-28,53],[-12,48],[9,93],[-1,88],[-11,54],[-26,13],[-28,103],[-18,101],[-33,35],[-13,136],[-37,123],[-13,138],[-18,21],[-16,108],[-23,90],[-31,55],[-20,76],[-4,138],[-11,112],[6,44],[-12,23],[11,66],[18,-37],[13,61],[10,171],[-21,148],[-19,54],[-11,-28],[-38,3],[-32,81],[-17,88],[-13,13],[-21,97],[-4,61],[5,129],[-16,155],[-16,34],[6,192],[-5,110],[29,35],[15,-95],[-11,-26],[5,-118],[32,-37],[48,-139],[13,3],[-19,50],[-14,195],[-23,98],[-25,34],[10,55],[-7,90],[-19,4],[-11,66],[18,14],[29,86],[-11,60],[-26,47],[-25,-35],[-4,-110],[14,-40],[-13,-17],[-2,-48],[18,-56],[-18,15],[6,-79],[-13,-8],[-32,91],[-17,-4],[-33,120],[-33,41],[-20,-33],[17,187],[-12,87],[7,15],[-15,68],[-11,-9],[-2,67],[-17,105],[-33,70],[-25,72],[-29,148],[-57,170],[-22,86],[-2,41],[12,112],[-13,130],[-10,37],[-8,144],[-7,35],[7,161],[10,77],[-8,160],[-8,33],[-8,135],[-16,33],[-6,55],[-29,120],[-13,19],[-8,86],[-22,32],[-48,151],[4,63],[-17,150],[22,201],[48,302],[13,176],[-14,54],[15,182],[12,244],[-5,126],[-23,224],[-25,44],[10,80],[2,177]],[[29842,64946],[5,31],[20,10],[-25,-41]],[[29251,64242],[-1,1]],[[29224,64083],[12,62],[3,82],[32,20],[9,-53],[-23,-85],[-33,-26]],[[27690,66134],[53,94],[40,94],[20,73],[60,82],[21,93],[5,54],[36,54],[14,40],[2,74],[-20,140],[-22,34],[1,94],[-19,14],[9,70],[-4,135],[69,81],[49,40],[40,20],[87,-5],[20,6],[96,-40],[51,-114],[14,-9],[32,39],[74,23],[29,-21],[28,2],[13,27],[30,18],[26,41],[18,78],[40,72],[21,55],[34,-10],[15,18],[9,53],[2,102],[-9,164],[-19,61],[19,36],[7,-26],[21,39],[-1,41],[13,72],[-23,-43],[-10,36],[23,57],[-9,30],[-26,0],[-8,-55],[21,10],[-15,-69],[-15,69],[4,39],[-19,14],[-2,34],[16,115],[18,6],[23,41],[1,48],[45,78],[24,24],[26,75],[15,97],[72,220],[38,91],[18,62],[46,70],[47,101],[18,0],[20,36],[29,-28],[13,11],[150,-9],[76,11],[148,11]],[[29390,64638],[-44,-153],[16,-98],[-2,48],[15,69],[18,-47],[-2,57],[25,44],[17,-47],[5,82],[30,-51],[-6,60],[13,-32],[35,-24],[22,27],[-3,46],[108,-4],[38,20],[67,133],[12,51],[34,24],[-30,-54],[22,-84],[27,13],[15,-69],[23,31],[18,59],[16,17],[8,-40],[-67,-93],[-99,-150],[-83,-95],[-68,-95],[-54,-57],[-33,3],[-69,-65],[-56,15],[-46,-55],[2,38],[-22,-2],[-9,59],[12,89],[-7,9]]],"bbox":[-179.148909,-14.548699,179.77847,71.365162],"transform":{"scale":[0.0035893096830968306,0.0008591472014720147],"translate":[-179.148909,-14.548699]}};
  
  var map = {};
  map.json = topojson.feature(state_topo, state_topo.objects.states);

  map.container = container;
 
  //keep track of the current map size, set in proj.dims
  map.size = null;

  map.metroData = metroData();
  
  //dom selections
  map.sel = {wrap: d3.select(map.container)};
  
  map.sel.mapwrap = map.sel.wrap.insert("div",":first-child").style("padding","0px");
  map.sel.svg = map.sel.mapwrap.append("svg").style("width","100%").style("height","100%");
  map.sel.g_state = map.sel.svg.append("g").attr("transform","translate(0,0)");
  map.sel.g_bubble = map.sel.svg.append("g").attr("transform","translate(0,0)");
  map.sel.g_anno = map.sel.svg.append("g").attr("transform","translate(0,0)");
 
  map.sel.tooltip = map.sel.mapwrap.append("div")
      .style("position","absolute")
      .style("display","block")
      .style("visibility","hidden")
      .style("width","300px")
      .style("height","300px")
      .style("pointer-events","none")
      .style("top","0px")
      .style("left","0px")
      .style("padding","5px")
      .style("border","1px solid #dddddd")
      .style("background-color","#ffffff")
      ;

  //create a non-visible area to test tooltip width without worrying about (browser-forced) text wrapping (when tooltip overflows window)
  var tiptest = map.sel.mapwrap.append("div")
      .style("position","absolute")
      .style("display","block")
      .style("visibility","hidden")
      .style("width","auto")
      .style("height","auto")
      .style("pointer-events","none")
      .style("top","0px")
      .style("left","0px")
      .style("padding",map.sel.tooltip.style("padding"))
      .style("border",map.sel.tooltip.style("border"))
      .style("background-color","#ffffff");

  var proj = {};
  proj.aspect = 0.66; //determines the height of the svg container
  proj.scale = 1.35; //scalar (of width) to be used for the Albers projection
  proj.fn = d3.geoAlbersUsa();
  proj.path = d3.geoPath().projection(proj.fn);
  proj.init = false;

  //get dimensions, update projection info
  proj.dims = function(){
    var box = map.sel.mapwrap.node().getBoundingClientRect();
    var d = {width:box.right-box.left};
    d.height = Math.round(proj.aspect*d.width);
    
    //set height of svg container
    map.sel.svg.style("height",d.height+"px");

    //set scale of projection
    proj.fn.scale(proj.scale*d.width).translate([d.width/2, d.height/2]);

    proj.init = true;

    map.size = d; //keep track of current size
    
    return d;
  };
  
  //hold redraw methods for the various layers in each map
  var redraw = {};

  //make map responsive -- redraw each layer
  map.responsive = function(){
    function resize(){
      var dims = proj.dims();
      for(var p in redraw){
        if(redraw.hasOwnProperty(p)){
          redraw[p]();
        }
      }
    }

    var resize_timer;
    window.addEventListener("resize", function(){
      clearTimeout(resize_timer);
      resize_timer = setTimeout(resize, 200);
    });

    return map;
  };

  //handle map data and state
  var map_data = {
  	metro:{
  		geo_id:null, 
  		data:null, 
  		color:null,
  		size:null,
  		max_radius:null, 
  		min_radius:null
  	}, 
  	state:{}, 
  	county:{},
  	store:{}
  };

  map.store = function(data, name){
  	if(arguments.length==0){
  		return map_data.store;
  	}
  	else if(arguments.length==1){
  		return map_data.store[data];
  	}
  	else{
  		map_data.store[name] = data;
  		return map;
  	}
  };



  //get, set data
  //accepts data in the format: [{cbsa:aaaaa, indicator1:xx.x, indicator2:yy.y}, {...}, ...]
  map.data = function(data, geo_id, geo_type){
  	if(arguments.length==0){return map_data}
  	else if(arguments.length < 3 || geo_type.toLowerCase() in {metro:1, met:1, cbsa:1, metropolitan:1}){
  		//metro data
  		if(!!geo_id){map_data.metro.geo_id = geo_id;}
      	map_data.metro.data = map.metroData.zip(data, map_data.metro.geo_id);
  	}	
  	return map;
  };

  //drawing functions 
  //these drawing functions shold execute with no arguments, in which case they redraw with the existing state
  //these may be called as methods of the redraw object, so be careful with use of this ref
  
  //TO DO - ADD VORONOI LAYOUT
  map.bubbles = map.bubble = function(size, color, max_bubble_size, min_bubble_size){
    //initialize dimensions, if first run (of any layer; do not reset dims for a layer redraw)
    if(!proj.init){proj.dims();}
    var vals = map_data.metro.data;

	//console.log(map_data);

    for(var i=0; i<vals.length; i++){
    	vals[i].xy = proj.fn(vals[i].id.lonlat);
    }

    //set min/max bubble radii values
    if(!!max_bubble_size){
    	map_data.metro.max_radius = max_bubble_size;
    }
    else if(map_data.metro.max_radius === null){
    	map_data.metro.max_radius = 25;
    }

    if(!!min_bubble_size){
    	map_data.metro.min_radius = min_bubble_size;
    }
    else if(map_data.metro.min_radius === null){
    	map_data.metro.min_radius = 2;
    }

    var s_default = function(s){
    	var v = map_data.metro.data.map(function(d){
    		return d.obs[s];
    	});
    	var extent = d3.extent(v);
    	var absmax = d3.max([Math.abs(extent[0]), extent[1]]);

    	var max_radius = map_data.metro.max_radius;
    	var min_radius = map_data.metro.min_radius;
    	
    	//interpolators
    	var i3 = d3.scaleSqrt().domain([0, absmax]).range([min_radius, max_radius]);
    	var i2 = d3.scaleSqrt().domain([0, absmax]).range([min_radius*0.6, max_radius*0.6]);
    	var i1 = d3.scaleSqrt().domain([0, absmax]).range([min_radius*0.3, max_radius*0.3]);

    	var p = function(v){
    		var w = map.size.width;
    		var i = w > 900 ? i3 : (w > 500 ? i2 : i1);
    		return i(v); 
    	};

    	//value to size
    	return function(obs){
    		var d = obs[s];
    		if(obs.hasOwnProperty(s) && d !== null){
    			return d >= 0 ? p(d) : p(Math.abs(d));
    		}
    		else{
    			return p(0);
    		}
    	}
    };

    var c_default = function(c){
    	var v = map_data.metro.data.map(function(d){
    		return d.obs[c];
    	});
    	var extent = d3.extent(v);
    	var absmax = d3.max([Math.abs(extent[0]), extent[1]]);

    	//interpolators
    	if(extent[0] < 0){
    		var p = d3.interpolateLab("#8ED7FF", "#0D73D6");
    		var n = d3.interpolateLab("#FF756F", "#DC2A2A");
    	}
    	else{
    		var p = d3.interpolateLab("#8ED7FF", "#0D73D6");
    		var n = null;
    	}

    	//value to color
    	return function(obs){
    		var d = obs[c];
    		if(obs.hasOwnProperty(c) && d !== null){
    			return d >= 0 ? p(d/absmax) : n(Math.abs(d)/absmax);
    		}
    		else{
    			return "#999999";
    		}
    	}
    };


    if(!!size){
    	map_data.metro.size = (typeof size == "function") ? size : s_default(size);
    }
    else if(map_data.metro.size === null){
    	map_data.metro.size = function(v){return 4};
    }

    if(!!color){
    	map_data.metro.color = (typeof color == "function") ? color : c_default(color);
    }
    else if(map_data.metro.color === null){
    	map_data.metro.color = function(v){return "#0d73d6";};
    }

    var sorted_data = map_data.metro.data
    								.slice(0)
    								.sort(function(a,b){
    									var a_size = map_data.metro.size(a.obs);
    									var b_size = map_data.metro.size(b.obs);
    									return b_size-a_size;
    								});

    var bub_update = map.sel.g_bubble.selectAll("circle").data(sorted_data, function(d){return d.id.code});
   	bub_update.exit().remove();
    var bub_merge = bub_update.enter()
    			   .append("circle")
    			   .attr("stroke-width","1")
    			   .merge(bub_update)
    			   .attr("cx", function(d,i){return d.xy[0]})
    			   .attr("cy", function(d,i){return d.xy[1]})
    			   .transition()
    			   .attr("r", function(d,i){return map_data.metro.size(d.obs)})
    			   .attr("fill", function(d,i){return map_data.metro.color(d.obs)})
    			   .attr("fill-opacity","0.9")
				   .attr("stroke", function(d,i){
				   		return d3.color(map_data.metro.color(d.obs)).darker();
				   });
				   //.order();


	//to do: consider

    //register
    if(!redraw.hasOwnProperty("bubble")){redraw.bubble = map.bubble;}

    return map;
  };

  map.states = map.state = function(){
    if(!proj.init){proj.dims();}
    var states = map.sel.g_state.selectAll("path").data(map.json.features);
	states.exit().remove();
    states.enter().append("path").classed("state",true)
                                 .merge(states)
                                 .attr("fill","none")
                                 .attr("stroke","#999999")
                                 .attr("stroke-width","0.5px")
                                 .attr("d", proj.path)
                                 ;

    //register
    if(!redraw.hasOwnProperty("state")){redraw.states = map.states;} 

    return map;
  };



  return map;

}

//basic card creation, registration of methods
function metro_map(container, url, state, indicators){

	//indicators should contain map titles and indicator keys
	//e.g. [{key:"employment", title:string OR function}, {...}, ...]

	//initialize card object
	/*var co = card(container)
	          .set_data(state.metro, "cbsa")
	          .set_data(state.period, "period")
	          .set_data(0, "init")
	          .build(buildMetMap);*/
	
	//setup
	var outer_wrap = d3.select(container).classed("makesans",true);
	var header_wrap = outer_wrap.append("div")
		.style("border-bottom","1px solid #aaaaaa")
		.append("div").classed("as-table",true)
		.append("div").classed("row-of three-two", true);

	var title_wrap = header_wrap.append("div").style("vertical-align","bottom");
	var button_wrap = header_wrap.append("div")
							.style("vertical-align","bottom")
							.classed("c-fix","true")
							.append("div")
							.style("float","right")
							.classed("c-fix",true)
							.style("margin","0em 0em 1em 0em");

	var title = title_wrap.append("div").style("margin","0em 0em 0.5em 0em").append("p").style("margin","0em").classed("chart-title",true);

	var wrap = outer_wrap.append("div");
	var map = metMap(wrap.node())
					.responsive()
					.states();


	//on button press, make cycling = false

	//button_wrap.append("p").style("clear","both").style("margin","0em 0em 0.5em 0em").text("Select an indicator to map: ")
	var buttons_update = button_wrap.selectAll("div").data(indicators);
	buttons_update.exit().remove();
	var buttons_enter = buttons_update.enter().append("div").classed("mi-button stacked disable-hightlight",true);
	buttons_enter.append("p");
	var buttons = buttons_enter.merge(buttons_update).style("margin","0px 1px 1px 0px");
	buttons.select("p").text(function(d,i){return d.title});		

	var cycling = false;
	var cycle_timer;
	buttons.on("mousedown", function(d,i){
		cycling = false;
		clearTimeout(cycle_timer);
		drawIndicator(i);
	});

	var init = false;
	var current_indicator_index = 0;
	function drawIndicator(index){
		//no checks performed as to whether map intiialized
		var i = index%(indicators.length);
		title.html("100 metro area map: " + indicators[i].title + ", " + state.period + ' year time frame<br /><span style="font-size:0.8em">To do: Clean up titles and time period labels. Add metro hover/selection indicator and enable hover-for-more-information. Scale bubbles consistently over time?</span>');
		var ind = indicators[i].key;
		current_indicator_index = i;
		map.bubble(ind,ind);
		buttons.classed("selected", function(d,j){return i==j});
		if(cycling){
			cycle_timer = setTimeout(function(){drawIndicator(++i);}, 2500);
		}
	}

	function activate(){
		d3.json(url, function(error, data){
			if(!!error){

			}
			else{
				map.store(data, "all_data");
				map.data(data[state.period], "cbsa");
				init = true;

				drawIndicator(0);
			}
		});
	}




	//drawIndicator(0);


	function updateMetro(){

	}

	function updatePeriod(){
		if(init){
			var data = map.store("all_data");
			map.data(data[state.period], "cbsa");
			drawIndicator(current_indicator_index);
		}
	}

	return {activate:activate, updateMetro:updateMetro, updatePeriod:updatePeriod}


}

//v2.0 developed for metro monitor
//revised to be more modular - each event gets a window event listener, rather than a collection
//the user registers scroll and activate listeners separately, scroll methods will not execute if there is an activation method yet to run

//events: activate and scroll 

//to do: address edge case where asynchronous load pushes unactivated element into view. no scroll takes place to activate it
//consider appending a note to each container element, but should we change positioning of parent element?

//scroll collection constructor
function onScroll(element){
	if(arguments.length > 0){
		this.element = element;
	}
	this.on_activate = null;
	this.on_scroll = null;

	this.activated = false;

	//determines activation zone. the default, 0.2, implies the middle 60% of the page is the "activation zone" 
	//the activate method will not be called when the top of element is 
	this.top_bot_buffer = 0.2;

	var self = this;
	var decorated_scroll_listener = function(){
		self.scroll_listener();

		//remove the scroll event if it is no longer necessary
		try{
			if(self.on_scroll === null && self.activated){
				window.removeEventListener("scroll", decorated_scroll_listener);
			}
		}
		catch(e){
			//ho-op
		}
	};

	//attach scroll listener with setTimeout 0 so that synchronous code, like registering activate/viewing listeners can execute first
	setTimeout(function(){
		window.addEventListener("scroll", decorated_scroll_listener);
	}, 0);
}

onScroll.prototype.buffer = function(buffer){
	if(arguments.length > 0){
		this.top_bot_buffer = buffer;
		return this;
	}
	else{
		return this.top_bot_buffer;
	}
};

onScroll.prototype.element = function(element){
	if(arguments.length > 0){
		this.element = element;
		return this;
	}
	else{
		return this.element;
	}
};

onScroll.prototype.get_box = function(){
	try{
		var box = this.element.getBoundingClientRect();

		var top = box.top;
		var bottom = box.bottom;
		var middle = top + ((bottom-top)/2);

		var pos = {top:top, middle:middle, bottom:bottom};
	}
	catch(e){
		var pos = null;
	}
	return pos;
};

//register activation function
onScroll.prototype.activate = function(fn){
	if(arguments.length > 0){
		this.on_activate = fn;
			var self = this;
			setTimeout(function(){self.scroll_listener();}, 0); //try to immediately activate
		return this;
	}
	else{
		return this.on_activate;
	}
};

//register scrolling function
onScroll.prototype.scroll = function(fn){
	if(arguments.length > 0){
		this.on_scroll = fn;
		return this;
	}
	else{
		return this.on_scroll;
	}
};
onScroll.prototype.scrolling = onScroll.prototype.scroll;

onScroll.prototype.scroll_listener = function(){
	var box = this.get_box();
	var window_height = Math.max(document.documentElement.clientHeight, (window.innerHeight || 0));
	
	//first, attempt to execute activate method, then scroll method--never at the same time
	if(!this.activated && this.on_activate !== null){
		if(box==null || window_height==0){
			this.on_activate({top:0, middle:0, bottom:0});
			this.activated = true;
		}
		else{
			var activate_zone = [window_height*this.top_bot_buffer, window_height*(1-this.top_bot_buffer)];	
			if(!(box.bottom < activate_zone[0] || box.top > activate_zone[1]) ){
				//console.log(box);
				this.on_activate(box);
				this.activated = true;
			}
		}
	}
	else if((this.on_scroll !== null) && !(box.bottom < 0 || box.top > window_height)){
		this.on_scroll(box);
	}
};

function waypoint(element){
	var os = new onScroll(element);
	return os;
}

function definitions(container){
	var colors = {};
	colors.background = "#eeeeee";
	colors.background_emphasis = "#dddddd";
	colors.strip = "transparent";
	colors.highlight = "#dc2a2a";
	colors.button = "#0d73d6";
	colors.button = "#999999";

	var state = {};
	state.wrap = container;

	state.defs = {};
	state.defs.growth = {};
	state.defs.prosperity = {};
	state.defs.inclusion = {};

	state.defs.growth.overall = "Growth indicators measure change in the size of a metropolitan area economy and its level of entrepreneurial activity. Growth and entrepreneurship create new opportunities for individuals and can help a metropolitan economy become more efficient. The Metro Monitor measures growth in gross metropolitan product, number of jobs, and number of jobs at young firms.";
	state.defs.prosperity.overall = "Prosperity captures changes in the average wealth and income produced by an economy. When a metropolitan area grows by increasing the productivity of its workers, through innovation or by upgrading workers’ skills, for example, the value of those workers’ labor rises. As the value of labor rises, so can wages. Increases in productivity and wages are what ultimately improve living standards for workers and families and the competitiveness of metropolitan economies.";
	state.defs.inclusion.overall = "Inclusion indicators measure how the benefits of growth and prosperity in a metropolitan economy—specifically, changes in employment and income—are distributed among individuals. Inclusive growth enables more people to invest in their skills and to purchase more goods and services. Thus, inclusive growth can increase human capital and raise aggregate demand, boosting prosperity and growth. Ensuring that all people can contribute to and benefit from growth and prosperity also helps sustain widespread support for the policies on which growth and prosperity depend.";

	state.defs.growth.indicators = [
		{
			title: "Gross metropolitan product (GMP)",
			definition: "Like gross domestic product, GMP measures the total value of goods and services produced in a metropolitan area, including wages and profits.", 
			source: "Moody's Analytics"
		},
		{
			title: "Jobs",
			definition: "The total number of occupied full- and part-time employment positions in a metropolitan economy.", 
			source: "Moody's Analytics"
		},
		{
			title: "Jobs at young firms",
			definition: "The total number of full- and part-time jobs at young, private-sector firms aged five years or less. This measures the employment impact of entrepreneurship and new ventures in a metropolitan area.", 
			source: "U.S. Census Bureau’s Longitudinal Employer-Household Dynamics program"
		}
	];

	state.defs.prosperity.indicators = [
		{
			title: "Standard of living",
			definition: "GMP, from above, divided by total metropolitan population yields GMP per capita, which reflects a metropolitan economy’s average standard of living.", 
			source: "Moody's Analytics and the U.S. Census Bureau"
		},
		{
			title: "Productivity",
			definition: "GMP, from above, divided by the total number of jobs, from above, yields average GMP per job, a crude measure of a metropolitan economy’s productivity.", 
			source: "Moody's Analytics"
		},
		{
			title: "Average annual wage",
			definition: "Aggregate annual wages paid to workers divided by the total number of jobs yields the average annual wage per job in a metropolitan economy.", 
			source: "Moody's Analytics"
		}
	];

	state.defs.inclusion.indicators = [
		{
			title: "Employment rate",
			definition: "The share of all individuals aged 18 to 65 who are employed.", 
			source: "U.S. Census Bureau"
		},
		{
			title: "Median wage",
			definition: "The annual wage earned by the person in the middle of a metropolitan area’s income distribution (among people at least 16 years old).", 
			source: "U.S. Census Bureau"
		},
		{
			title: "Relative poverty",
			definition: "The share of people earning less than half of the local median wage (among people at least 16 years old).", 
			source: "U.S. Census Bureau"
		}
	];


	state.build = function(container){
		if(!!container){state.wrap = container;}
		try{
			var wrap = d3.select(state.wrap)
						 .style("margin","0em 0em")
						 .style("padding","0em")
						 .classed("c-fix",true);

			var buttonbox = wrap.append("div").classed("c-fix",true)
											  .style("padding","0px")
											  .style("border", "0px solid #dc2a2a")
											  .style("background-color",colors.strip)
											  .style("border-width", "0px 0px");
			var buttons = buttonbox.selectAll("div").data([
							{key:"growth", value:"Growth"},
							{key:"prosperity", value:"Prosperity"},
							{key:"inclusion", value:"Inclusion"}])
							.enter().append("div")
							.style("float","left")
							.style("cursor","pointer")
							.style("padding","0.5em 1em 0.5em 1em")
							.classed("disable-highlight", true)
							.style("margin","0px 1px 1px 0px");
							
			var buttontext = buttons.append("p")
							.style("margin","0em")
							.style("font-weight","bold")
							.text(function(d,i){return d.value});

			var defwrapper = wrap.append("div")
								.style("background-color", colors.background)
								.classed("row-of two-three", true);

			var titlebox = defwrapper.append("div")
								.style("padding","1em 1em 2em 1em")
								.style("background-color",colors.background_emphasis);
			
			var contentbox = defwrapper.append("div")
											.style("padding","0em")
											.append("div")
											.style("padding","1em 1em 2em 1em")
											.style("margin-left","0px")
											.append("div")
											.classed("c-fix row-of three-cells",true);
			function sync(d,i){
				buttons.style("background-color", function(e,j){
					return i===j ? colors.highlight : colors.button;
				});

				buttontext.style("color", function(e, j){
					return i===j ? "#ffffff" : "#ffffff";
				}).style("font-size","1.15em");

				var title = titlebox.selectAll("p").data([state.defs[d].overall]);
				title.enter().append("p").merge(title).text(function(d,i){
					return d;
				}).style("font-weight","normal")
				.style("font-size","1em")
				.style("margin","0em");
				
				var defboxes = contentbox.selectAll("div").data(state.defs[d].indicators);

				var defs = defboxes.enter().append("div")
									.merge(defboxes)
									.selectAll("p").data(function(d,i){
										return [d.title, d.definition, d.source]
									});
				defs.enter().append("p").merge(defs)
				.style("font-weight", function(d,i){
					return i===0 ? "bold" : "normal";
				})
				.style("margin","0em 0em 0em 0em")
				.classed("", function(d,i){return i===0})
				.classed("source-text", function(d,i){return i===2})
				.text(function(d,i){
					return i===2 ? "Source: " + d : d 
				});

			}

			buttons.on("mousedown", function(d,i){
				sync(d.key, i);
			});

			sync("growth", 0);
		}
		catch(e){
			console.log("Error loading definitions, please reload page.");
		}
	};


	return state;
}

//args
//container: div to make the header wrap
//parent (optional): specify the element that wraps the header
	//useful if you want to fix the header to the bottom of the parent element when it (the bottom) is in view
	//enables multiple headers on a single page
	//makes parent element relative positioned

function header(){
	var O = {};

	var height = 91;
	var height_fixed = 46;
	var parent = null;
	var container = null;

	var background_color = "#eeeeee";
	var background_color_fixed = "#dddddd";

	var num_args = arguments.length;

	O.height = function(h0, h1){
		if(arguments.length > 0){
			height = h0+1;
			height_fixed = !!h1 ? h1+1 : height;
			return O;
		}
		else{
			return height;
		}
	};

	O.parent = function(p){
		if(arguments.length > 0){
			parent = p;
			d3.select(parent).style("position","relative");
			return O;
		}
		else{
			return parent;
		}

	};

	O.build = function(container_element, callback){
		if(arguments.length > 0){
			container = this.wrap = container_element;

			var wrap = d3.select(container).style("height", height+"px")
										  .style("width","100%")
										  .classed("fixed-menu",true);

			var inner =  wrap.append("div").style("height", height+"px")
										   //.style("background-color","#fafafa")
										   .style("background-color",background_color)
										   .style("z-index","100")
										   //.style("border","1px solid #dc2a2a")
										   //.style("border-width","1px 0px 1px 0px")
										   .classed("c-fix",true);

			var content = inner.append("div")
						.classed("content-box full-bleed",true);

			this.content = content;

			//track the header: 0-default, 1-fixed, 2-absolute
			var position = 0;

			//scroll event handler
			function pos(){

				var window_height = Math.max(document.documentElement.clientHeight, (window.innerHeight || 0));

				var past_bottom = (!!parent && parent.getBoundingClientRect().bottom < window_height-height_fixed) ? true : false;

				var rect = container.getBoundingClientRect();
				try{
					if(rect.bottom < 0 && !past_bottom){
						if(position !== 1){
							inner.interrupt()
								 .style("position","fixed")
								 .style("bottom", (-height_fixed+"px"))
								 .style("height", height_fixed+"px")
								 .style("background-color",background_color_fixed)
								 .style("top","auto")
								 .style("left","0px")
								 .style("width","100%")
								 .transition()
								 .duration(400)
								 .style("bottom","-1px")
								 //.on("end", function(d,i){
								 	//force repaint. sometime transition results in a 1px gap
								 //	inner.style("bottom","-1px").style("display","block");
								 //})
								 ;
							position = 1;
						}
					}
					else if(rect.bottom < 0 && past_bottom){
						if(position !== 2){
							inner.interrupt()
								 .transition()
								 .duration(0)
								 .style("position","absolute")
								 .style("bottom","auto")
								 .style("height",height+"px")
								 .style("background-color",background_color)
								 .style("top","calc(100% + 2em)")
								 .style("left","0px")
								 .style("width","100%")
								 ;
							position = 2;
						}
					}
					else{
						inner.interrupt().transition().duration(0)
								 .style("position","relative")
								 .style("width","auto")
								 .style("height",height+"px")
								 .style("background-color",background_color)
								 .style("top","auto")
								 .style("bottom","auto")
								 ;
						position = 0;
					}
				}
				catch(e){
					inner.style("position","relative").style("width","auto");
				}
			}

			//set up in next tick
			setTimeout(function(){
				pos();
				window.addEventListener("scroll", pos);
				window.addEventListener("resize", pos);
			}, 0);
			
				/*pos();
				window.addEventListener("scroll", pos);*/

		}

		return O;
	};

	return O;
}

//v1.0 developed for congressional district poverty, gig economy, and gci summit

var format = {};
format.rank = function(r){
	try{
	    if(r == null){
	        throw "badInput";
	    }
	    else{
	        
	        var c = r + "";
	        var f = +(c.substring(c.length-1)); //take last letter and coerce to an integer
	         
	        var e = ["th","st","nd","rd","th","th","th","th","th","th"];
	 
	        var m = (+r)%100; 
	        var r_ = (m>10 && m<20) ? c + "th" : (c + e[f]); //exceptions: X11th, X12th, X13th, X14th
	    }
	}
	catch(e){
	    var r_ = r+"";
	}

	return r_; 
};

//percent change
format.pct0 = d3.format("+,.0%");
format.pct1 = d3.format("+,.1%");

//percent change
format.ch0 = d3.format("+,.0f");
format.ch1 = d3.format("+,.1f");

//shares
format.sh0 = d3.format(",.0%");
format.sh1 = d3.format(",.1%");

//numeric
format.num0 = d3.format(",.0f");
format.num1 = d3.format(",.1f");
format.num2 = d3.format(",.2f");
format.num3 = d3.format(",.3f");

//USD
format.doll0 = function(v){return "$" + format.num0(v)};
format.doll1 = function(v){return "$" + format.num1(v)};
format.doll2 = function(v){return "$" + format.num2(v)};

format.dolle30 = function(v){return "$" + format.num0(v*1000)};

//id
format.id = function(v){return v};

//possessive
format.possessive = function(v){
	var s = v+"";
	var last = s.slice(-1).toLowerCase();
	return last=="s" ? s+"'" : s+"'s";
};

//wrapper that handles missings/nulls
format.fn = function(v, fmt){
	if(format.hasOwnProperty(fmt)){
		var fn = format[fmt];
	}
	else{
		var fn = format.id;
	}
	return v==null ? "N/A" : fn(v);
};

//similar to fn above, but returns a decorated function instead of a value
format.fn0 = function(fmt){
	if(format.hasOwnProperty(fmt)){
		var fn = format[fmt];
	}
	else{
		var fn = format.id;
	}
	return function(v){
		return v==null ? "N/A" : fn(v);
	}
};

//to do: json data for this chart type needs metadata for each indicator/measure. specifically: var name and format
//also z-score ranges to use -- or should that be calculated below?
//incorporate metro highlighting, test update method
//pass in the state/scope object that sets initial selections and update selections for metro/time period/etc/
//methods need to gracefully handle missing state properties

//to do: consider ditching card api and simpifying update procedure: 
// 1) update data (binding -- expensive, often involves data fetching/manipulation),
// 2) update state (e.g. metro selection, color -- soft update, no data rebind)
// 3) update size (resize)

function ranks_and_distributions(container, url, state){

  var colors = {};
  colors.dot = "#0d73d6";
  colors.stroke = "#033CED";

  var metname = metro_select().lookup;

  //chart setup
  var chart = {};
  chart.init = false;
  chart.width = 320;
  chart.height = 550;
  chart.wrap_outer = d3.select(container).classed("card-box makesans",true);
  chart.textwrap = chart.wrap_outer.append("div").style("padding","2em 1em");
  chart.wrap = chart.wrap_outer.append("div").classed("content-box", true).append("div"); //.style("margin","1em 10%");
  chart.label_container = chart.wrap.append("div")
                                    .classed("c-fix",true)
                                    .style("display","table")
                                    .style("width","100%")
                                    .append("div")
                                    .style("display","table-row");
                                    

  chart.svg = chart.wrap.append("svg")
                    .style("height", chart.height+"px")
                    .style("width","100%");

  chart.main_group = chart.svg.append("g");
  chart.anno_group = chart.svg.append("g");
	chart.axis_label = chart.svg.append("g");
	chart.sub_svg = chart.svg.append("svg")
                       .style("width","48%")
                       .style("height","50%")
                       .attr('x',"52%")
                       .attr("y","0");

  chart.anno_dot = chart.anno_group.append("circle").attr("stroke-width","2px");

  chart.xaxis = chart.main_group.append("line")
                                .attr("stroke","#999999")
                                .style("shape-rendering","crispEdges")
                                .attr("stroke-width","1px");

  var metro_data = metroData();

  //build the actual chart
  function build(){
    var self = this;
    var period = this.get_data("period");
    var cbsa = this.get_data("cbsa");

    //to do -- build data based on metadata
    var indicators = this.get_data().measures;

    var data = indicators.map(function(d,i,a){
      var D = self.get_data().values[period][d.key];
      var Z = metro_data.zip(D, "cbsa", "geo"); //bind to official names
      var I = d;
      return {variable:d, data:Z}
    });

    var thisBox = container.getBoundingClientRect();
    var width = Math.round(thisBox.right - thisBox.left);

    //to do: figure this out -- build from metadata?
    var cat = data[0].data.concat(data[1].data, data[2].data);

    //var zrange = [-2,4];
    var zrange = d3.extent(cat, function(d){return d.obs.z});



    var x = d3.scaleBand().domain(indicators.map(function(d){return d.key})).range([0,100])
          .round(false).paddingOuter(0).align(0.5);
    var y = d3.scaleLinear().domain(zrange).range([chart.height-10, 10]);

    var po = x.paddingOuter();
    var pi = x.paddingInner();
    var step = x.step();
    var halfstep = step/2;

    chart.xaxis.attr("x1",(halfstep*po)+"%")
               .attr("x2",(100-(halfstep*po))+"%")
               .attr("y1",y(0))
               .attr("y2",y(0))
               ;

    //add in labels
    var labels_update = chart.label_container.selectAll("p").data(indicators);
    var labels_enter = labels_update.enter().append("p")
                                .classed("small-labels", true)
                                .style("text-align","center")
                                .style("margin","0px 4px")
                                .style("word-break","normal");

    var labels = labels_enter.merge(labels_update).style("display","table-cell").style("vertical-align","bottom");
    labels.style("margin","0em")
      .style("width",step+"%")
      .style("overflow","hidden")
        .html(function(d,i){return "<span>"+d.value+"</span>"});


    //build groups
    var groups_update = chart.main_group.selectAll("g.group-dot-array").data(data);
    var groups_enter = groups_update.enter().append("g").classed("group-dot-array",true)
                                    .style("pointer-events","all");
        //groups_enter.append("rect");
        groups_enter.append("g").classed("dot-group",true).attr("transform","translate(-15,0)");
        groups_enter.append("line");
    var groups = groups_enter.merge(groups_update);

    groups.select("rect")
          .attr("x",function(d,i){
            var indicator = d.variable.key; //to do: add in metadata to json
            return x(indicator)+"%";
          })
          .attr("y",0)
          .attr("height", chart.height)
          .attr("width", step+"%")
          .attr("fill", "none");

    groups.select("line")
          .attr("x1",function(d,i){return (x(d.variable.key)+halfstep)+"%"})
          .attr("x2",function(d,i){return (x(d.variable.key)+halfstep)+"%"})
          .attr("y1",0)
          .attr("y2",chart.height)
          .attr("stroke","#aaaaaa")
          .attr("stroke-dasharray","1,1")
          .style("shape-rendering","crispEdges");

    var dots = groups.select("g.dot-group").selectAll("rect").data(function(d,i){
      return d.data.filter(function(d,i,a){return d.obs.z !== null});
    });
    dots.exit().remove();
    var alldots = dots.enter().append("rect").merge(dots)
            .attr("x",function(d,i){return (x(d.obs.measure)+halfstep)+"%"})
            .attr("y",function(d,i){
              return d.id.code == cbsa ? y(d.obs.z)-1 : y(d.obs.z)
            })
            .attr("fill", function(d,i){
              return d.id.code == cbsa ? "#dc2a2a" : colors.dot
            })
            .attr("fill-opacity",function(d,i){
              return d.id.code == cbsa ? 1 : 0.5;
            })
            .attr("stroke",colors.dot)
            .attr("stroke-width",0)
            .attr("stroke-opacity",0.5)
            .attr("height", function(d,i){
              return d.id.code == cbsa ? 3 : 1;
            })
            .attr("width",function(d,i){
              return d.id.code == cbsa ? 42 : 30;
            })
            .style("shape-rendering","crispEdges");

    //BUILD OUT TEXT COMPONENT
    (function(){
      try{
        var overall = self.get_data().ranks[period];
        var values = self.get_data().values[period];
        var indicators = self.get_data().measures;

        var lookup = {};
        var o = overall.filter(function(d,i){
          return d.cbsa == cbsa;
        });
        lookup.cbsa = cbsa;
        lookup.rank = o.length==1 ? o[0].rnk : null;
        
        lookup.values = indicators.map(function(d,i){
          var v = values[d.key].filter(function(obs){
            return obs.cbsa == cbsa;
          }); 
          return v.length==1 ? {rank:v[0].rank, val:v[0].val, z:v[0].z, variable:d.value, var:d.key} : null;
        });

        //console.log(lookup);

        var txt = chart.anno_group.selectAll("text").data(lookup.values);
        txt.enter().append("text").merge(txt)
            .attr("x",function(d,i){return (x(d.var)+halfstep)+"%"})
            .attr("dx",-20)
            .attr("y",function(d,i){return y(d.z)+6})
            .attr("text-anchor","end")
            .text(function(d,i){return format.pct1(d.val)});

        var boxtext = chart.textwrap.selectAll("p.data-text").data([lookup.rank].concat(lookup.values));
        boxtext.enter().append("p").classed("data-text",true).merge(boxtext).text(function(d,i){
          if(i==0){
            return format.possessive(metname(cbsa).nameshort) + " overall rank: " + format.rank(d);
          }
          else{
            return d.variable + ": " + format.pct1(d.val) + " (" + format.rank(d.rank) + ")"
          }
        })
        .classed("big-strong", function(d,i){return i==0});

        var legendtext = chart.textwrap.selectAll("p.legend-text").data(["Add in text and symbols describing chart", "Each line depict one of the 100 largest metro areas. " + metname(cbsa).nameshort + " is highlighted in red.", "The solid line is the 100-metro average. (Data have been standardized)"]);
        legendtext.enter().append("p").classed("legend-text",true).merge(legendtext).text(function(d,i){return d}).style("font-style","italic");
        
      }
      catch(e){
        //roll back everything
      }
    })();

    /////code from gci2016

    /*axis_label.append("line").attr("x1",20)
                 .attr("x2",140)
                 .attr("y1",y(0)+15)
                 .attr("y2",y(0)+15)
                .attr("stroke","#aaaaaa")
                .attr("stroke-width","1px")
                .attr("fill","none")
                .style("shape-rendering","crispEdges");

    axis_label.append("path").attr("d", "M140,"+(y(0)+15)+" m0,-5 l10,5 l-10,5 z")
                .attr("stroke","#aaaaaa")
                .attr("stroke-width","1px")
                .attr("fill","#aaaaaa");

    axis_label.selectAll("text").data([1,2]).enter().append("text").classed("above-avg-label",true)
          .text("Above average")
          .attr("x", 20)
          .attr("y", y(0))
          .attr("dy",13)
          .attr("dx",10)
          .style("font-size","13px")
         .attr("fill",function(d,i){return i==0 ? "#ffffff" : "#666666"})
        .attr("stroke",function(d,i){return i==0 ? "#ffffff" : "none"})
        .attr("stroke-width",function(d,i){return i==0 ? "3px" : "0px"});

    axis_label.attr("transform","rotate(-90 10 " + y(0) + ")");*/
    ////end gci code

    chart.init = true;
  }

  //initialize card object
  var co = card(container)
              .set_data(state.metro, "cbsa")
              .set_data(state.period, "period")
              //.printErrors(false)
              .build(build);

  //activate the chart on scrolled
  function activate(){
    co.json(url);
    //window.addEventListener("resize", function(){co.build()});
  }

  function updateMetro(){
    co.set_data(state.metro, "cbsa").build();
  }

  function updatePeriod(){
    co.set_data(state.period, "period").build();
  }


  //activate will actually fetch the data and run the build function
  //update will rebuild with new cbsa and/or time period selection -- build is a no-op if the default json data hasn't been loaded for the card yet
  return {
    activate:activate, 
    updateMetro:updateMetro,
    updatePeriod:updatePeriod, 
    container:container
  }

}

//TO DO: investigate d3 tween for absolutely positioned dives with left % length

//shift share charts
function shift_shares(container, url, state){

	//chart setup
  	var chart = {};
  	chart.init = false;

	//colors for trend lines
	chart.cols = {};

	//chart.height
	chart.height = 325;

	chart.wrap_outer = d3.select(container)
		.classed("c-fix makesans",true);

	//title
	chart.title = chart.wrap_outer.append("p").classed("chart-title",true)
	.html('Did job growth in the ' + state.metro_lookup(state.metro).nameshort + 
		' metro area outperform or underperform the nation?<br /><span style="font-size:0.8em"> To do: Annotate with job growth numbers and percentages, clarify definitions, and main takeaway. Add a table below the aggregate shift share, showing 2015 industry (2-, 3-, or 4-digit?) shares, change, and comparison to national growth.</span>');
	
	//hold chart in wrap
	chart.wrap = chart.wrap_outer.append("div")
		.style("position","relative")
		.style("padding","1em 1em")
		.style("background-color","#eeeeee")
		.append("div")
		.style("position","relative")
		.style("padding","0px")
		.style("margin","0px");

	//legend
	/*chart.legend = chart.wrap_outer.append("div").classed("c-fix",true);
	chart.legend_entries = chart.legend.selectAll("div").data(["Metro","U.S."]).enter()
	      .append("div").style("float","left").style("margin","0px 10px 0px 0px").classed("c-fix",true);
	chart.legend_entries.append("div")
	.style("height","2px")
	.style("width","20px")
	.style("margin","7px 5px 6px 2px")
	.style("float","left")
	.style("background-color",function(d,i){
	  return i===0 ? chart.cols.metro : chart.cols.us;
	});
	chart.legend_entries.append("p").text(function(d,i){
	return d;
	})
	.style("line-height","1em")
	.style("margin","0px")
	.style("float","left");*/

	function build(){
		self = this;
		var json = self.get_data();
		var metro = this.get_data("cbsa");
		var metroName = state.metro_lookup(metro);

		var maxPctCh = 0.10;

		var data = json[state.period].filter(function(d,i,a){
			return (d.cbsa+"") == metro;
		})[0];

		//set scales -- not consistent between metros
		var shifts = {cs:data.cs/data.e0, im:data.im/data.e0, ns:data.ns/data.e0};

		var e0 = data.e0;
		var shifts = [
						{val:data.ns, label:"ns"}, 
						{val:data.im, label:"im"},
						{val:(data.ns+data.im), label:"pro"},
						{val:data.e1-data.e0, label:"act"},
						{val:data.cs, label:"cs"}
					];

		var max = d3.max(shifts, function(d,i){return d.val});
		var min =  d3.min(shifts, function(d,i){return d.val});
		var absmax = Math.max(Math.abs(max), Math.abs(min))/e0;

		/*if(absmax < maxPctCh){
			var extent = [0-maxPctCh, maxPctCh];
		}
		else{
			var extent = [0-absmax, absmax];
		}*/
		var extent = min > 0 ? [0, max/e0] : (max < 0 ? [min/e0, 0] : [min/e0, max/e0]);

		//map data to % range
		var scaleX = d3.scaleLinear().domain(extent).range([45,95]);
		var zero = Math.round(scaleX(0));

		var detail = json.detail[metro];

		var bar_height = (chart.height / 4) - 50; 
		if(bar_height < 1){bar_height = 1;}

		var barsU = chart.wrap.selectAll("div.bar-wrap").data(shifts);
		barsU.exit().remove();
		var barsE = barsU.enter().append("div").classed("bar-wrap",true);
		barsE.append("div")
			.classed("value-bar",true)
			.style("top","0.25em")
			.style("min-height","1.5em")
			.style("height","90%")
			.style("max-height","4em")
			.style("position","absolute")
			;

		barsE.append("p");

		var bars = barsE.merge(barsU)
			.style("position","relative")
			.style("z-index",0)
			.style("margin","0.5em 0em 0.5em 0em")
			.style("padding","0em")
			.style("min-height","85px")
			//.style("background-color","#eeeeee")
			.style("border-bottom",function(d,i){return i < 4 ? "2px solid #ffffff" : "none"});

		var barsV = bars.select("div.value-bar")
			.style("left",function(d,i){
				var x = scaleX(d.val/e0);
				var w = Math.round(Math.abs(scaleX(d.val/e0) - zero));
				if(x >= zero){
					var l = zero+"%";
				}
				else{
					var l = (zero-w)+"%";
				}
				return l;
			})
			.style("width",function(d,i){
				var w = Math.round(Math.abs(scaleX(d.val/e0) - zero));
				return w+"%";
			})
			.style("background-color",function(d,i){
				return d.val < 0 ? "#dc2a2a" : "#0d73d6";
			})
			.style("height","30px");

		var ydiv_u = chart.wrap.selectAll("div.shift-axis").data([0]);
			ydiv_u.exit().remove();
		var ydiv_e = ydiv_u.enter().append("div")
									.classed("shift-axis",true)
									.style("width","1px")
									.style("border","0px solid #ffffff")
									.style("top","-0.5em")
									.style("margin","0px")
									.style("height","calc(100% + 1em)")
									.style("background-color","#333333")
									.style("position","absolute")
									.style("z-index",1);
		var ydiv = ydiv_e.merge(ydiv_u).style("left",zero+"%");

		var text = bars.select("p").html(function(d,i){
			if(i==0){
				return "<strong>(a) National growth effect</strong>: The job change that would occur if the metro area matched total national job growth.";
			}
			else if(i==1){
				return "<strong>(b) Industry mix effect</strong>: The job change that would occur if every industry in the metro area matched the national growth rate in that industry, less the national growth effect."
			}
			else if(i==2){
				return "<strong>(c) Expected job growth</strong>: The sum of the national growth effect and the industry mix effect (a + b). It is the change that would occur if the metro area's industries matched the national performance of those industries."
			}
			else if(i==3){
				return "<strong>(d) Actual job growth</strong>: The actual job change in the metro area."
			}
			else if(i==4){
				return "<strong>(e) Metro area competitive shift</strong>: The difference between the actual job change and the expected job change (d - c). Indicates whether the metro area overperformed or underperformed given its industrial structure."
			}


		}).style("line-height","1.35em")
		  .style("font-size","0.8em")
		  .style("padding","7px 10px")
		  .style("max-width","40%");

		(function(){
			var heights = [];
			bars.each(function(d,i){
				var box = this.getBoundingClientRect();
				var h = Math.round(box.bottom-box.top);
				heights.push(h);
			});
			var min = d3.min(heights) + "px";

			barsV.style("max-height", "calc(" + min + " - 1em)");			
		})();

	}

	//initialize card object
	var co = card(container)
	          .set_data(state.metro, "cbsa")
	          .set_data(state.period, "period")
	          .build(build);

	//closure for use in setTimeout below
	function resizeBuild(){
		co.build();
	}

	//activate the chart on scrolled
	function activate(){
		co.json(url);
		var resizeTimer;
		window.addEventListener("resize", function(){
			clearTimeout(resizeTimer);
			resizeTimer = setTimeout(resizeBuild, 150);
		});
	}

	function updateMetro(){
		co.set_data(state.metro, "cbsa").build();
		chart.title.html('Did job growth in the ' + state.metro_lookup(state.metro).nameshort + 
		' metro area outperform or underperform the nation?<br /><span style="font-size:0.8em"> To do: Annotate with job growth numbers and percentages, clarify definitions, and main takeaway. Add a table below the aggregate shift share, showing 2015 industry (2-, 3-, or 4-digit?) shares, change, and comparison to national growth.</span>');
	
	}

	function updatePeriod(){
		co.set_data(state.period, "period").build();
	}

	return {
	  activate:activate, 
	  updateMetro:updateMetro,
	  updatePeriod:updatePeriod, 
	  container:container
	}
}

//to do: test output of metro lookup function

function trend_lines(container, url, state){

  //chart setup
  var chart = {};
  chart.init = false;

  //colors for trend lines
  chart.cols = {};
  chart.cols.us = "#aaaaaa";
  chart.cols.metro = "#0d73d6";

  //individual plot heights
  chart.plotheight = 95;
  chart.plotpad = 45;
  chart.three_chart_pad = 0;


  chart.wrap_outer = d3.select(container).classed("card-box makesans",true);
  
  chart.wrap = chart.wrap_outer.append("div").classed("content-box", true).append("div"); //.style("margin","1em 10%");
  chart.textwrap = chart.wrap_outer.append("div").style("padding","2em 0em 2em 2em");
  chart.title = chart.textwrap.append("p").classed("big-strong",true);


  //title and legend
  chart.legend = chart.textwrap.append("div").classed("c-fix",true);
  chart.legend_entries = chart.legend.selectAll("div").data(["Metro","U.S."]).enter()
          .append("div").style("float","left").style("margin","0em 1em 1.5em 1em").classed("c-fix",true);
  chart.legend_entries.append("div")
    .style("height","3px")
    .style("width","30px")
    .style("margin","7px 5px 6px 2px")
    .style("float","left")
    .style("background-color",function(d,i){
      return i===0 ? chart.cols.metro : chart.cols.us;
    });
  chart.legend_entries.append("p").text(function(d,i){
    return d;
  })
  .style("line-height","1em")
  .style("margin","0px")
  .style("float","left");

  chart.textwrap.append("p").text("Add a little text comparing the trajectory of the selected metro versus the U.S. during the chosen time period (1 yr, 5yr, 10yr.")
    .style("font-style","italic")
    ;

  //wrap to hold actual plots
  chart.label_container = chart.wrap.append("div")
                                    .classed("c-fix",true)
                                    .style("width","100%");

  chart.svg = chart.wrap.append("svg")
                    .style("height", ((chart.plotheight*3)+(chart.plotpad*2)+35+65)+"px")
                    .style("width","100%")
                    .append("g").attr("transform","translate(45,35)");


  //add a single xaxis group
  chart.xaxis = chart.svg.append("g")
                         .attr("transform","translate(0," +  ((3*chart.plotheight)+(2*chart.plotpad)+10)+")")
                         .classed("d3-axis-group",true);

  //hold vertical panels to activate mouseenter events
  chart.hover_panels = chart.svg.append("g");

  //add a chart note
  chart.note = chart.wrap.append("p")
    .text("Data unavailable for 2000–05; values are interpolated for these years.")
    .style("font-size","13px")
    .style("margin","-10px 10px 0px 45px")
    ;



 ///FROM 2016 MetroMonitor
function build(){
  var self = this;
  var json = this.get_data();
  var metro = this.get_data("cbsa");
  var metroName = state.metro_lookup(metro);
  var years = json.years;
  var indicators = json.measures;
  var category = json.category;
  
  var data = indicators.map(function(d,i,a){
    var met = json.values[d.key][metro];
    var us = json.values[d.key]['99999'];
    return {metro: !!met ? met : {values:[]},
            us: !!us ? us : {values:[]},
            indicator:d}
  });

  chart.title.html("Trends in the components of " + category.toLowerCase());

  chart.legend_entries.select("p")
                       .text(function(d,i){return i===0 ? metroName.nameshort : "United States"});
  chart.note.style("display",category==="inclusion" ? "block" : "none");

  //set chart dimensions
  try{
    var box = chart.wrap.node().getBoundingClientRect();
    var width = Math.round(box.right-box.left);
    if(width < 300){throw "BadDimensions"}
  }
  catch(e){
    var width = 500;
  }
  finally{
    var chartWidth = width-95;
    //charts.groupselect("rect.chart-back").attr("width",chartWidth);
  }

  //x-scaling
  var scaleX = d3.scaleLinear().domain(d3.extent(years)).range([0,chartWidth]);
  var axisX = d3.axisBottom(scaleX)
               .tickValues(years)
               .tickFormat(function(v){return v})
               .tickSize(6,6);

  //data mapper
  var dataMapper = function(d,i){
    var all = d.metro.values.concat(d.us.values);
    var extent = d3.extent(all, function(d,i){return d.val});
    var extent2 = [
                    extent[0]-(Math.abs(extent[0])*0.05), extent[1]+(Math.abs(extent[1])*0.05)
                  ];
    var scaleY = d3.scaleLinear().domain(extent2).range([chart.plotheight, 0]);
    var tickVals = scaleY.ticks(3);

    var axisY = d3.axisLeft(scaleY)
                 .tickValues(tickVals)
                 .tickFormat(function(v){return v})
                 .tickSize(6,6);

     var y = function(obs){return scaleY(obs.val)};
     var x = function(obs){return scaleX(obs.year)};

     var line = d3.line().x(x).y(y).defined(function(d,i){
      var badyear = typeof d.year === "undefined" || d.year === null;
      var badval = typeof d.val === "undefined" || d.val === null;
      return !(badyear || badval);
     });

     var metpath = line(d.metro.values);
     var uspath = line(d.us.values);

     return {dmet: metpath===null ? "M0,0" : metpath,
             dus: uspath===null ? "M0,0" : uspath,
             ticks:tickVals,
             yscale:scaleY,
             yaxis:axisY,
             l:d.indicator.value};
  };


  //plot groups
  var chart_groups = chart.svg.selectAll("g.plot-group").data(data.map(dataMapper));
  var chart_groups_enter = chart_groups.enter().append("g")
  .classed("plot-group", true);


  //one-time additions
    //add y-axes to each group
    chart_groups_enter.append("g").classed("d3-axis-group",true).attr("transform","translate(0,0)");

    //add the basic chart elements to each group
    chart_groups_enter.append("g").classed("grid-line-group",true);
    chart_groups_enter.append("path").classed("us-trend-line",true)
      .attr("d","M0,0").attr("stroke",chart.cols.us);
    chart_groups_enter.append("path").classed("metro-trend-line",true)
      .attr("d","M0,0")
      .attr("stroke", chart.cols.metro)
      ;
    chart_groups_enter.append("text")
                .classed("chart-title",true)
                .attr("x",0)
                .attr("y",-10)
                .attr("font-size","1em")
                .text("...");

    chart_groups_enter.append("g")
                      .style("visibility","hidden")
                      .classed("chart-hover-dot-group",true);

  chart.groups = chart_groups_enter.merge(chart_groups)
                                .attr("transform",function(d,i){
                                  return "translate(0," + i*(chart.plotheight+chart.plotpad) + ")";
                                });

chart.groups.select("path.metro-trend-line").attr("d",function(d,i){
 return d.dmet;
}).attr("fill","none")
  .attr("stroke-width","3px")
  .attr("stroke-linecap","round");
chart.groups.select("path.us-trend-line").attr("d",function(d,i){
 return d.dus;
}).attr("fill","none")
  .attr("stroke-width","2px");

chart.groups.select("text.chart-title")
      .text(function(d,i){return d.l})
      .attr({x:0, "text-anchor":"start", "font-weight":"bold"});

var gridLines = chart.groups.select("g.grid-line-group").selectAll("line").data(function(d,i){
  return d.ticks.map(function(v,i){return d.yscale(v) });
});
gridLines.exit().remove();
gridLines.enter()
         .append("line")
         .merge(gridLines)
         .attr("x1",0)
         .attr("x2",chartWidth)
         .attr("stroke","#dddddd")
         .attr("stroke-width","1")
         .attr("stroke-dasharray","2,2")
         .attr("y1",function(d,i){return d})
         .attr("y2",function(d,i){return d})
         .style("shape-rendering","crispEdges");


//build axes

 try{
   var yaxes = chart.groups.select("g.d3-axis-group").each(function(d,i){
     d3.select(this).transition().call(d.yaxis);
   });
 }catch(e){
   //console.log(e);
 }

 try{
   chart.xaxis.call(axisX)
       .selectAll("text")
       .attr("transform","rotate(-45)")
       .attr("dy","8px")
       .attr("dx","-0.2em")
       .style("font-weight","normal")
       .attr("text-anchor","end");
 }
 catch(e)
 {

 }




return null;








   //add hover panels
   var verticalBlinds = charts.hover.selectAll("rect").data(ticks);
   var vertBlindWidth = chartWidth/(ticks[ticks.length-1]-ticks[0]);
   verticalBlinds.enter().append("rect");
   verticalBlinds.exit().remove();
   verticalBlinds.attr({"height":"800px", "fill":"#ffffff", "opacity":"0", "stroke":"#ffffff", "y":-40, width:vertBlindWidth}).style("pointer-events","all")
     .attr("x",function(d,i){
       return scaleX(d)-(0.5*vertBlindWidth);
     });

   var dotGroup;
   verticalBlinds.on("mouseenter",function(d,i){
     try{
       var x = scaleX(d);
       var obs = data.filter(function(o,i,a){
         return o.year==d;
       });
       var usobs = usdata.filter(function(o,i,a){
         return o.year==d;
       });

       //each chart group (3 total) has 1 hover dot group -- we need to bind the accessors with the dot data.
       dotGroup = charts.groups.selectAll("g.chart-hover-dot-group").data(function(d,i){
         return [{acc:d, dat:[usobs[0],obs[0]], col:["#aaaaaa","#00649f"] }];
       });
       dotGroup.enter().append("g").classed("chart-hover-dot-group",true);
       dotGroup.exit().remove();

       dotGroup.attr("transform","translate("+x+",0)").style("visibility","visible");

       var backs = dotGroup.selectAll("rect").data(function(d,i){return [d,d]});
         backs.enter().append("rect");
         backs.exit().remove();
         backs.attr({"x":0,"fill":"#ffffff","opacity":0.75});

       var dots = dotGroup.selectAll("circle").data(function(d,i){return [d,d]});
         dots.enter().append("circle");
         dots.exit().remove();
         dots.attr({"cx":"0","r":"3"})
           .attr("cy",function(d, i){
             return d.acc.y(d.dat[i]);
           })
           .attr("fill",function(d,i){
             return d.col[i];
           });

       var labels = dotGroup.selectAll("text").data(function(d,i){return [d,d]});
         labels.enter().append("text");
         labels.exit().remove();
         labels.attr({"x":0})
             .text(function(d,i){
               var other = i==0 ? 1 : 0; //index of the other label
               var val = d.acc.val(d.dat[i]);
               var otherval = d.acc.val(d.dat[other]);
               return i==0 && val==otherval ? "" : d.acc.fmt(val);
             })
             .attr("text-anchor","start")
             .attr("y",function(d,i){
               var other = i==0 ? 1 : 0; //index of the other label
               var val = d.acc.val(d.dat[i]);
               var otherval = d.acc.val(d.dat[other]);
               var y = d.acc.yscale(val);
               if(val >= otherval){var yp = y-7;}
               else{var yp = y+14;}
               return yp;
             })
             .attr("fill",function(d,i){
               return i==0 ? "#666666" : d.col[i];
             })
             .style("fill",function(d,i){
               return i==0 ? "#666666" : d.col[i];
             });

       backs.each(function(d,i){
         try{
           var text = d3.select(this.parentNode).selectAll("text").filter(function(d,j){return i==j});
           var box = text.node().getBoundingClientRect();
           var w = box.right - box.left;
           var y = text.attr("y");
           d3.select(this).attr({"width":box.right-box.left+6, "height":box.bottom-box.top+4, "x":-1, "y":y-13, "rx":5, "ry":5});
         }
         catch(e){
           console.log(e);
           d3.select(this).style("width","0");
         }

       });

     }
     catch(e){
       charts.groups.select("g.chart-hover-dot-group").style("visibility","hidden").selectAll("text").remove();
     }
   });
   verticalBlinds.on("mouseleave",function(d,i){
     dotGroup.attr("transform","translate(0,0)").style("visibility","hidden");
   });

 }
 ///END FROM 2016


  //initialize card object
  var co = card(container)
              .set_data(state.metro, "cbsa")
              .set_data(state.period, "period")
              .build(build);

  //activate the chart on scrolled
  function activate(){
    co.json(url);
    window.addEventListener("resize", function(){co.build();});
  }

  function updateMetro(){
    co.set_data(state.metro, "cbsa").build();
  }

  function updatePeriod(){
    co.set_data(state.period, "period").build();
  }

  //activate will actually fetch the data and run the build function
  //update will rebuild with new cbsa and/or time period selection -- build is a no-op if the default json data hasn't been loaded for the card yet
  return {
      activate:activate, 
      updateMetro:updateMetro,
      updatePeriod:updatePeriod, 
      container:container};
}

function section_header(g_scope){
	var lookup = metro_select().lookup;
	var updates = {};
	var numHeaders = -1;

	var build = function(container, title, subtitle_fn){
		var id = "H" + (++numHeaders);
		var wrap = d3.select(container)
			.classed("makesans",true)
			.style("padding","1em 1em 0.5em 1em")
			.style("margin","6em 0em 3em 0em")
			.style("border-bottom", "1px solid rgb(220, 42, 42)");
		var arglen = arguments.length;

		var title = wrap.append("p")
							.style("font-size","2em")
							.style("font-weight","bold")
							.style("text-align","center")
							.style("color","rgb(220, 42, 42)")
							.style("margin","0em")
							.text(title);

		if(subtitle_fn !== null){
			var subtitle = wrap.append("p")
								.style("font-size","1em")
								.style("text-align","center")
								.style("color","rgb(220, 42, 42)")
								.style("text-transform","uppercase")
								.style("margin","0em");

			var subfn = function(){
				var html = arglen < 3 ? "In the <strong>" + 
									lookup(g_scope.metro).name + 
									"</strong> metropolitan area" :
									subtitle_fn(lookup(g_scope.metro));				
				return html;
			};


			subtitle.html(subfn());

			updates[id] = function(){
				subtitle.html(subfn());
			};
		}
	};

	var buildMain = function(container){
		var id = "H" + (++numHeaders);

		var text = function(){
			return "The " + lookup(g_scope.metro).nameshort + " metropolitan area";
		};

		var wrap = d3.select(container)
						.style("margin","6em 0em 0em 0em");

		var wrap = d3.select(container)
			.classed("makesans",true)
			.style("padding","1em 1em 0.5em 1em")
			.style("margin-bottom","0em")
			.style("border-bottom", "1px solid rgb(220, 42, 42)")
			.append("div")
			.classed("content-box",true)
			.style("margin","0px auto");

		wrap.append("p")
			.text("Explore data for")
			.style("font-weight","bold")
			.style("color","#dc2a2a")
			.style("margin","0em")
			;

		var p = wrap.append("p")
			.style("font-weight","bold")
			.style("color","#dc2a2a")
		 	.style("font-size","2em")
		 	.style("margin", "0em 0em 0.2em 0em")
		 	.style("line-height","1.2em")
		 	.text(text());	
	
		updates[id] = function(){
			p.text(text());
		};
	};

	var updatefn = function(){
		for(var p in updates){
			if(updates.hasOwnProperty(p)){
				updates[p]();
			}
		}
	};

	return {factory:build, factorySpecial:buildMain, update:updatefn};

}

//left off here

function time_select(){
	var sel = {};

	sel.setup = function(container){

		var odata = [{period:"one", label:"2014–2015"},{period:"five", label:"2010–2015"},{period:"ten", label:"2005–2015"}];


		var wrap = d3.select(container).classed("makesans",true);
			wrap.selectAll("*").remove(); //there's no updating here

		var instruction = wrap.append("p").html("Time period: ")
										  .style("display","inline-block")
										  .style("margin","0em")
										  .style("line-height","1em")
										  .style("font-size","0.8em")
										  .style("padding","0px 0px 0px 6px")
										  //.style("color","#dc2a2a")
										  .style("text-transform","uppercase");

		var select = wrap.append("select").style("display","block")
										  .style("margin","0px 0px 0px 0px")
										  .style("line-height","1.65em")
										  .style("font-size","1em")
										  .style("padding","2px 5px 2px 5px")
										  .style("background","transparent")
										  //.style("color","#dc2a2a")
										  //.style("border-color","#dc2a2a")
										  .style("outline","none");

		this.node = select.node();

		var options = select.selectAll("option").data(odata).enter().append("option");
			options.attr("value", function(d,i){return d.period})
				   .text(function(d,i){return d.label})
				   .style("color","#333333");

		select.on("change", function(d,i){
			var val = this.value;
			try{
				var m = odata[this.selectedIndex];

				if(m.period!==val){
					throw "ERROR";
				}
			}
			catch(e){
				var m = odata[0];
				sel.update(m.period);
			}

			if(!!sel.onchg){
				sel.onchg(m);
			}
		});
		return sel;
	};

	//update the selection, but don't trigger change event
	sel.update = function(val){
		if(this.node){
			this.node.value = val;
		}

		return sel;
	};
	//the callback executes with the sel object as this
	sel.onchange = function(callback){
		sel.onchg = callback;

		return sel;
	};

	return sel;
}

//add browser compat message: test for svg, array.filter and map

//To do: review handling of missing values in all modules: both implicit and explicit missings

//shared js-modules
//project-specific js-modules
//window.addEventListener("scroll", function(){console.log("SCROLL...")});

dir.add("json");
dir.add("json17", "data/2017/json");

//for now, run the app locally
dir.local("./");

var overall = dir.url("json", "coreIndicators.json");
var combined = dir.url("json", "combinedIndicators.json");



//KEEP TRACK OF ALL CARDS AND SCROLL EVENTS HERE
var cards = {};

//KEEP TRACK OF CURRENT METRO AND TIME PERIOD
var scope = {};
scope.metro = "47900";
scope.period = "five";

var sectionHeader = section_header(scope);


function main(){

	//TESTING OF SCROLL
	/*(function(){
		window.addEventListener("scroll", function(){console.log("scrolled")})

		var id = ["one","two","three","four","five","six"];
		var i = -1;

		function expand(){
			if(++i < id.length){
		    var el = document.getElementById(id[i]);
		    el.style.height = "600px"
		    setTimeout(expand, 1000);
		   }
		}

		expand();
	})();*/
	//END TESTING OF SCROLL



	var mhead = header()
			.parent(document.getElementById("mm-wrap"))
			.height(220, 65)
			.build(document.getElementById("mm-header"));

	var header_content = mhead.content
							.append("div")
							.style("padding", "5px 0em 0px 0em")
							.classed("c-fix content-box",true);

	var iselect_wrap = header_content.append("div")
			.style("float","left")
			.style("margin","0em")
			.classed("c-fix",true);
	iselect_wrap.append("p").text("Overall rankings will go here").style("margin","5px 0em 0em 0em");
	iselect_wrap.selectAll("div.ranking-tile").data([1,2,3]).enter().append("div")
			.style("float","left")
			.style("width","100px")
			.style("height","80px")
			.style("border","1px solid #aaaaaa")
			.style("margin","0.25em 1em 1em 0em")
			;

	var tselect_wrap = header_content.append("div")
			.style("float","right")
			.style("margin","0em 0em 0em 2em");

	var mselect_wrap = header_content.append("div")
		.style("float","right");

	//headers
	sectionHeader.factory(document.getElementById("growth-section-header"), "Growth");
	sectionHeader.factory(document.getElementById("prosperity-section-header"), "Prosperity");
	sectionHeader.factory(document.getElementById("inclusion-section-header"), "Inclusion");
	
	sectionHeader.factorySpecial(document.getElementById("data-section-header"));


	//build select menu, and use its metro name lookup function
	var mselect = metro_select().setup(mselect_wrap.node()).update(scope.metro);
	var tselect = time_select().setup(tselect_wrap.node()).update(scope.period);


	setTimeout(function(){
		mselect.onchange(function(d){
			var sel = this;
			scope.metro = d.CBSA_Code;
			cards.growth1.updateMetro();
			cards.growth2.updateMetro();
			cards.growth3.updateMetro();
			cards.growth4.updateMetro();

			cards.prosperity1.updateMetro();
			cards.prosperity2.updateMetro();
			cards.prosperity3.updateMetro();

			cards.inclusion1.updateMetro();
			cards.inclusion2.updateMetro();
			cards.inclusion3.updateMetro();

			sectionHeader.update();
		});

		tselect.onchange(function(d){
			var sel = this;
			scope.period = d.period;
			cards.growth1.updatePeriod();
			cards.growth2.updatePeriod();
			cards.growth3.updatePeriod();
			cards.growth4.updatePeriod();

			cards.prosperity1.updatePeriod();
			cards.prosperity2.updatePeriod();
			cards.prosperity3.updatePeriod();

			cards.inclusion1.updatePeriod();
			cards.inclusion2.updatePeriod();
			cards.inclusion3.updatePeriod();
		});
	},0);

								
	scope.metro_lookup = mselect.lookup;

	//all cards go below this line__________________

	//attach all card activate functions to a single scroll container? try both
	cards.growth1 = ranks_and_distributions(document.getElementById("growth-card1"),
													dir.url("json17", "growth_change.json"),
													scope);

	//activate is the onscroll method that loads data and draws
	cards.growth1.activate();


	cards.growth2 = trend_lines(document.getElementById("growth-card2"),
													dir.url("json17", "growth_values.json"),
													scope);

	cards.growth2.activate();


	var growth_card_3 = document.getElementById("growth-card3");
	cards.growth3 = metro_map(growth_card_3, 
								dir.url("json17", "growth_map_data.json"),
								scope, 
								[{key:"emp", title:"Employment"},
								 {key:"output", title:"Output"},
								 {key:"emp5u", title:"Employment in new firms"}
								]);

	waypoint(growth_card_3)
		.activate(function(){
			cards.growth3.activate();
		})
		.scrolling(function(pos){
			//console.log("scrolling by..." + JSON.stringify(pos));
		})
		.buffer(0.1);
		
		//.viewing(function(){
		//	console.log("VIEWING!!!");
		//});


	var growth_card_4 = document.getElementById("growth-card4");
	cards.growth4 = shift_shares(growth_card_4, dir.url("json17", "shiftshare.json"), scope);

	waypoint(growth_card_4)
		.activate(function(){
			cards.growth4.activate();
		})
		.buffer(0.1);

	//metro_map(document.getElementById("growth-card3")).responsive();

	//build definitions menu
	definitions(document.getElementById("mm-defs")).build();

	//PROSPERITY
	cards.prosperity1 = ranks_and_distributions(document.getElementById("prosperity-card1"),
													dir.url("json17", "prosperity_change.json"),
													scope);

	//activate is the onscroll method that loads data and draws
	cards.prosperity1.activate();


	cards.prosperity2 = trend_lines(document.getElementById("prosperity-card2"),
													dir.url("json17", "prosperity_values.json"),
													scope);

	cards.prosperity2.activate();


	var prosperity_card_3 = document.getElementById("prosperity-card3");
	cards.prosperity3 = metro_map(prosperity_card_3, 
								dir.url("json17", "prosperity_map_data.json"),
								scope, 
								[{key:"gdpjob", title:"GDP per job"},
								 {key:"gdpcap", title:"GDP per capita"},
								 {key:"avgwage", title:"Average annual wage"}
								]);

	waypoint(prosperity_card_3)
		.activate(function(){
			cards.prosperity3.activate();
		})
		.scrolling(function(pos){
			//console.log("scrolling by..." + JSON.stringify(pos));
		})
		.buffer(0.1);	


	//INCLUSION
	cards.inclusion1 = ranks_and_distributions(document.getElementById("inclusion-card1"),
													dir.url("json17", "inclusion_change.json"),
													scope);

	//activate is the onscroll method that loads data and draws
	cards.inclusion1.activate();


	cards.inclusion2 = trend_lines(document.getElementById("inclusion-card2"),
													dir.url("json17", "inclusion_values.json"),
													scope);

	cards.inclusion2.activate();


	var inclusion_card_3 = document.getElementById("inclusion-card3");
	cards.inclusion3 = metro_map(inclusion_card_3, 
								dir.url("json17", "inclusion_map_data.json"),
								scope, 
								[{key:"medearn", title:"Median earnings"},
								 {key:"relpov", title:"Relative income poverty"},
								 {key:"epop", title:"Employment-to-population ratio"}
								]);

	waypoint(inclusion_card_3)
		.activate(function(){
			cards.inclusion3.activate();
		})
		.scrolling(function(pos){
			//console.log("scrolling by..." + JSON.stringify(pos));
		})
		.buffer(0.1);	

}

document.addEventListener("DOMContentLoaded", main);

}());
