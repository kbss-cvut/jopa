package cz.cvut.kbss.jopa.oom;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;

class InstanceRegistry {

	private static final URI defaultContext = URI.create("http://defaultContext");

	protected Map<URI, Map<URI, Object>> instances = new HashMap<>();

	<T> void registerInstance(URI primaryKey, T instance, URI context) {
		assert primaryKey != null;
//		context = checkForDefaultContext(context);

		if (!instances.containsKey(context)) {
			instances.put(context, new HashMap<URI, Object>());
		}
		instances.get(context).put(primaryKey, instance);
	}

	boolean containsInstance(URI primaryKey, URI context) {
//		context = checkForDefaultContext(context);
		return instances.containsKey(context) && instances.get(context).containsKey(primaryKey);
	}

	<T> T getInstance(Object primaryKey, URI context) {
//		context = checkForDefaultContext(context);
		if (instances.containsKey(context) && instances.get(context).containsKey(primaryKey)) {
			return (T) instances.get(context).get(primaryKey);
		}
		return null;
	}

	void reset() {
		this.instances = new HashMap<>();
	}

	protected URI checkForDefaultContext(URI context) {
		if (context == null) {
			context = defaultContext;
		}
		return context;
	}
}
