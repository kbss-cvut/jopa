package cz.cvut.kbss.owlpersistence.sessions;

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

	public PrivilegedInstanceCreator(Constructor<?> constructor) {
		this.constructor = constructor;
	}

	public Object run() throws Exception {
		return constructor.newInstance((Object[]) null);
	}

}
