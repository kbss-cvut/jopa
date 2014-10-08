package cz.cvut.kbss.jopa.oom;

import java.net.URI;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

class PendingChangeRegistry extends InstanceRegistry {

	void removeInstance(URI primaryKey, URI context) {
//		context = checkForDefaultContext(context);
		final Map<URI, Object> contextInstances = instances.get(context);
		if (contextInstances != null) {
			contextInstances.remove(primaryKey);
		}
	}

	Map<URI, Map<URI, Object>> getInstances() {
		final Map<URI, Map<URI, Object>> result = new HashMap<>();
		for (URI ctx : instances.keySet()) {
			if (!instances.get(ctx).isEmpty()) {
				result.put(ctx, Collections.unmodifiableMap(instances.get(ctx)));
			}
		}
		return result;
	}
}
