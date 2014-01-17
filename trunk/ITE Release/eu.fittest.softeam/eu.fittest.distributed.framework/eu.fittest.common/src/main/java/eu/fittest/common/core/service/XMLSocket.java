package eu.fittest.common.core.service;

import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PushbackInputStream;
import java.io.UnsupportedEncodingException;
import java.net.Socket;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CodingErrorAction;
import java.util.Arrays;
import java.util.logging.Level;
import eu.fittest.common.util.ITELogger;

import eu.fittest.common.core.exception.FITTESTException;


public class XMLSocket {

    private static final int BUFFER_SIZE = 1024;

    private static final char[] CHAR_TO_SKIP = {'\u0000','\r','\n'};

    static {
        Arrays.sort(CHAR_TO_SKIP);
    }
    
    private Socket _socket;
    private PushbackInputStream _in;
    private OutputStream _out;
    private char[] _buffer;
    private byte[] _bbuffer;
    private int _position;
    private boolean _readingHeader = false;
    private StringBuilder _message = null;
    private int _nbRead;
    private boolean _nextClosingAngleBracket = false;
    private CharsetDecoder _utfdecoder;
    
    public OutputStream getOutputStream(){
    	return _out;
    }
    
    public InputStream getInputStream() throws IOException{
    	_in.unread(_bbuffer,_position,_nbRead-_position);
    	_position = 0;
    	_buffer= new char[0];
    	return _in;
    }

    public XMLSocket(Socket socket) throws IOException {
    	_socket = socket;
    	_message = new StringBuilder();
        _in = new PushbackInputStream(_socket.getInputStream(), BUFFER_SIZE);
        _out = new BufferedOutputStream(_socket.getOutputStream());
    	_nbRead = 0;
    	_position = 0;
    	_buffer= new char[0];
    	_utfdecoder = Charset.forName("UTF-8").newDecoder().onMalformedInput(CodingErrorAction.REPORT).onUnmappableCharacter(CodingErrorAction.REPORT);
    }

    public void write(String message) throws FITTESTException{
    	ITELogger.info("sent: "+message+" to "+_socket.getRemoteSocketAddress());       
        try {
			_out.write((message+'\u0000').getBytes("UTF-8"));
	        _out.flush();
		} catch (UnsupportedEncodingException e) {
			throw new FITTESTException(e.getMessage());
		} catch (IOException e) {
			throw new FITTESTException(e.getMessage());
		}

    }
    
    private char[] readFromInputStream() throws IOException{
    	char[] charBuffer = new char[0];
    	_bbuffer= new byte[BUFFER_SIZE];
    	
    	do{
    		_position = 0;
    		_nbRead = _in.read(_bbuffer);
			if(_nbRead==-1) throw new IOException("connection to "+_socket.getRemoteSocketAddress()+" is closed");
			try{
			  charBuffer = _utfdecoder.decode(ByteBuffer.wrap(_bbuffer,0,_nbRead)).array();
			}
			catch(Exception e){
				_in.unread(_bbuffer,0,_nbRead);
				_bbuffer = new byte[_bbuffer.length*2];
			}
        }while(charBuffer.length==0);
    	return charBuffer;
    }
    
    public String read() throws IOException{
        int stack = 0;       
 
        do{
          if(_position>=_buffer.length){//everything has been read in the buffer
        	  _buffer = readFromInputStream();
          }
          
          do{
        	  if(stack==0 && _readingHeader){
        		  _readingHeader = false;
        	  }
        	  
              if(Arrays.binarySearch(CHAR_TO_SKIP,_buffer[_position])<0) {//if it is not a char to skip
                  switch(_buffer[_position]){
                      case '<':
                          stack++;
                          break;
                  	  case '?':
                  		  if(!_readingHeader && _message.length()>0 && _message.charAt(_message.length()-1)=='<'){
                  			  _readingHeader = true;
                  		  }
                  		  break;
                      case '/':
                          if(_message.length()>0 && _message.charAt(_message.length()-1)=='<'){
                              stack--;
                              _nextClosingAngleBracket = true;
                          }
                          break;
                      case '>':
                    	  if(_message.length()>0 && (_message.charAt(_message.length()-1)=='/' || _message.charAt(_message.length()-1)=='?')){
                              stack--;
                          }
                          else if(_nextClosingAngleBracket){
                              _nextClosingAngleBracket = false;
                              stack--;
                          }
                          break;
                  }     
                  
                 _message.append(_buffer[_position]);
                  
                  ITELogger.log(Level.FINEST, "currentChar["+_buffer[_position]+"];stack="+stack+";nextClosingAngleBracket="+_nextClosingAngleBracket+";"+_message.toString()+";"+_message.length());
                  
                  if(stack<0){
                      ITELogger.log(Level.SEVERE, "stack is negative");
                      throw new AssertionError(this.getClass().getName()+": stack is negative");
                  }
              }              
              _position++;
          }while(_position<_buffer.length && (stack!=0 || _readingHeader || _message.length()==0));
          
          ITELogger.log(Level.FINEST, _message.toString());
        }while(stack!=0 || _readingHeader || _message.length()==0);
        
        if(_position<_buffer.length) _position++;//skip \u0000, important for binary mode otherwise there is a shift
        else _in.skip(1);
        
        ITELogger.log(Level.INFO, "received: "+_message.toString()+" from "+_socket.getRemoteSocketAddress());
        
        String message = _message.toString();
        _message = new StringBuilder();
        return message;
    }

	public void close() throws IOException {
		_socket.close();
	}
	
	public boolean isClosed(){
		return _socket.isClosed();
	}
	
	public boolean isConnected(){
		return _socket.isConnected();
	}
	
	

}
