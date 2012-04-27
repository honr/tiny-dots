/**
 * junixsocket
 *
 * Copyright (c) 2009 NewsClub, Christian Kohlschutter
 *
 * The author licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package afunix;

import java.io.File;
import java.io.FileDescriptor;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * JNI connector to native JNI C code.
 * 
 * @author Christian Kohlschuetter
 */
final class NativeUnixSocket {
  static { 
    System.loadLibrary("NativeUnixSocket"); }

  static boolean isSupported() {
    return true;
  }
    
  native static void bind(final String socketFile, final FileDescriptor fd,
			  final int backlog) throws IOException;

  native static void listen(final FileDescriptor fd, final int backlog)
    throws IOException;

  native static void accept(final String socketFile,
			    final FileDescriptor fdServer, final FileDescriptor fd)
    throws IOException;

  native static void connect(final String socketFile, final FileDescriptor fd)
    throws IOException;

  native static int read(final FileDescriptor fd, byte[] b, int off, int len)
    throws IOException;

  native static int write(final FileDescriptor fd, byte[] b, int off, int len)
    throws IOException;

  native static void close(final FileDescriptor fd) throws IOException;

  native static void shutdown(final FileDescriptor fd, int mode)
    throws IOException;

  native static int getSocketOptionInt(final FileDescriptor fd, int optionId)
    throws IOException;

  native static void setSocketOptionInt(final FileDescriptor fd,
					int optionId, int value) throws IOException;

  native static void unlink(final String socketFile) throws IOException;

  native static int available(final FileDescriptor fd) throws IOException;

  native static void initServerImpl(final AFUNIXServerSocket serverSocket,
				    final AFUNIXSocketImpl impl);

  native static void setCreated(final AFUNIXSocket socket);

  native static void setConnected(final AFUNIXSocket socket);

  native static void setBound(final AFUNIXSocket socket);

  native static void setCreatedServer(final AFUNIXServerSocket socket);

  native static void setBoundServer(final AFUNIXServerSocket socket);

  native static void setPort(final AFUNIXSocketAddress addr, int port);
}
