   
	use LWP::Simple;
	use LWP::UserAgent;
	use HTTP::Request;
	use HTTP::Response;
	use HTML::LinkExtor; 
        use URI;


sub callurl{

    my $url = shift || die "url not passed";
    my $p   = shift || die "parameters not passed";
    my %params = %$p;
    my $f   = shift || die "filed value pairs not passed";
    my %Fields = %$f;
    my $v;
    my $k;
    my $method;


my $response;
my $contents;
            while ( ($k,$v) = each %params ) {
             if ($v eq '$_POST')
              {if ($method eq "" || $method eq 'post')
                 {$method = 'post';}
               else 
	       { die "error mixing submit types";}
              }
             else
              {if ($v eq '$_GET'  )
                {if ($method eq "" || $method eq 'get')
                   {$method ='get';}
                else
                   {die "mixing submit types";}
                }    
             }
            }

       #    print "@{[ %Fields ]}\n";
            if (lc($method) eq 'get')
              {
               $response = $browser->get($url,\%Fields);}
            else
              { 
               $response = $browser->post($url,\%Fields);}
	    

if ($response->is_error()) 
{print "\n", $response->status_line; }

$contents = $response->content();

print $contents;

}

    1;
