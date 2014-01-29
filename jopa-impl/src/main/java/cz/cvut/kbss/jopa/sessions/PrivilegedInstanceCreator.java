package cz.cvut.kbss.jopa.sessions;

import java.lang.reflect.Constructor;
import java.security.PrivilegedExceptionAction;

/**
 * Builds new instance with privileged access using the specified constructor.
 * 
 * @author kidney
 * 
 */
public class PrivilegedInstanceCreator implements
		PrivilegedExceptionAction<Object> {

	private Constructor<?> constructor;
	private Object[] params;

	public PrivilegedInstanceCreator(Constructor<?> constructor) {
		this.constructor = constructor;
		this.params = null;
	}

	public PrivilegedInstanceCreator(Constructor<?> constructor,
			Object... params) {
		this.constructor = constructor;
		this.params = params;
	}

	public Object run() throws Exception {
		return constructor.newInstance(params);
	}

}
