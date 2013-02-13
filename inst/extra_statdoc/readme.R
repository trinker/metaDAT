<p>Tools for data management, reshaping and basic transformations in meta-analysis</p>

<h2>Installation</h2>

<p>Currently there isn&#39;t a release on <a href="http://cran.r-project.org/">CRAN</a>.</p>

<p>You can, however, download the <a href="https://github.com/trinker/metaDAT">zip ball</a> or <a href="https://github.com/trinker/metaDAT">tar ball</a>, decompress and run <code>R CMD INSTALL</code> on it, or use the <strong>devtools</strong> package to install the development version:</p>

<pre><code class="r"># install.packages(&quot;devtools&quot;)

library(devtools)
install_github(&quot;metaDAT&quot;, &quot;trinker&quot;)
</code></pre>

<p>Windows users currently must install <code>RCurl</code> before installing metaDAT.  Use the following short script:</p>

<pre><code class="r">URL &lt;- &quot;http://www.stats.ox.ac.uk/pub/RWin/bin/windows/contrib/2.15/&quot;
install.packages(&quot;RCurl&quot;, contriburl = URL)
</code></pre>

<h2>Installing JabRef</h2>

<p><a href="http://trinker.github.com/metaDAT/">metaDAT</a> utilizes 
<a href="http://jabref.sourceforge.net/">JabRef</a> as a graphical 
user interface for data entry.  You will need to install a working copy from:</p>

<p><a href="http://sourceforge.net/projects/jabref/files/jabref/2.9.2/">http://sourceforge.net/projects/jabref/files/jabref/2.9.2/</a></p>


<p>Download the development version of metaDAT <a href="https://github.com/trinker/metaDAT/">here</a> 