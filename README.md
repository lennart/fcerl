fcerl
=====
The erlang flavoured Flash Compiler SHell powered by [ernie](http://github.com/mojombo/ernie).  
__fcerl__ provides a __BERTRPC__ interface to the FCSH.

Motivation
----------
I can't use up/down arrows in fcsh, I simply do not want to type the full mxmlc command for compiling Flex/Air projects by hand, copy paste sucks and I had problems with the current FCSH interface in [project-sprouts](http://github.com/lukebayes/project-sprouts). So I just hacked together a small native ernie module that does the nasty work for me.

Installation
------------
Get and install [erlang](http://erlang.org/download.html)  
as well as the [Flex SDK](http://opensource.adobe.com/wiki/display/flexsdk/Download+Flex+4) (I just tested it with version 4 beta)

    gem install ernie

and then

    ernie -c fcsh.cfg -p 7890

Usage
-----

Choose one of several available [__BERTRPC__ clients](http://bert-rpc.org/) e.g. for ruby.

Run this to start a FCSH process

    svc = BERTRPC::Service.new "localhost", 7890
    svc.call.fcsh.start

and then

    response = svc.call.fcsh.compile(your_awesome_mxmlc_command_as_a_string)

check for errors and print the response

    unless (status = response.shift) == :error
      puts response
    else
      raise response.unshift status
    end

and you're done. The response is a __BERT::Tuple__ that contains each line of the fcsh response, so you can simply `join "\n"` it for output.  
Subsequent compile requests will use incremental compilation, so compilation should only take a few seconds.

Current limitations & freaking weird stuff
------------------------------------------
* Be less verbose, please!  
* <del>Don't hardcode compile ids, currently only the first command will be able to leverage from the incremental compilation. </del> Fixed with latest commit
* better, or at least some kind of integration with project sprouts

