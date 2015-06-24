---
layout: default
title: FAO Regional Statistical Yearbooks 2015
---

## Intro

<img src="https://trakcel.com/images/process-design-1.png" width=150 style="float: right;"/>

This is a website that facilitates the FAO Regional yearbook process in 2015

<div class="home">

  <h3>Recent updates</h3>

  <ul class="posts">
    {% for post in site.posts %}
      <li>
        <span class="post-date">{{ post.date | date: "%b %-d, %Y" }}</span>
        <a class="post-link" href="{{ post.url | prepend: site.baseurl }}">{{ post.title }}</a>
      </li>
    {% endfor %}
  </ul>

  <p class="rss-subscribe">subscribe <a href="{{ "/feed.xml" | prepend: site.baseurl }}">via RSS</a></p>

</div>


Please email <a href="mailto:markus.kainu@fao.org?Subject=FAO regional yearboks" target="_top">Markus.Kainu@fao.org</a> if you have questions about the site.