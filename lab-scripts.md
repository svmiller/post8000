---
layout: page
title: Lab Scripts
permalink: /lab-scripts/
---

This page contains link to lab scripts throughout the semester. Clicking the title of the lab script will go directly to the ["spun"](https://deanattali.com/2015/03/24/knitrs-best-hidden-gem-spin/) HTML document from the underlying R code. The bottom right icons link to the underlying R script (<i class="fab fa-r-project"></i>) and the HTML document (<i class="fas fa-file-code"></i>).


<ul id="archive">
{% for lectures in site.data.labscripts %}
      <li class="archiveposturl">
        <span><a href="{{ site.url }}/lab-scripts/{{ lectures.filename }}.html">{{ lectures.title }}</a></span><br>
<span class = "postlower">
<strong>tl;dr:</strong> {{ lectures.tldr }}</span>
<strong style="font-size:100%; font-family: 'Titillium Web', sans-serif; float:right; padding-right: .5em">

<a href="https://github.com/{{ site.githubdir}}/tree/master/lab-scripts/{{ lectures.filename}}.R"><i class="fab fa-r-project"></i></a>&nbsp;&nbsp;
<a href="{{ site.url }}/lab-scripts/{{ lectures.filename }}.html"><i class="fas fa-file-code"></i></a>
</strong> 
      </li>
{% endfor %}
</ul>
