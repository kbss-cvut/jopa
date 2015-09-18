package cz.cvut.kbss.jopa.oom;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;

class InstanceRegistry {

    protected Map<URI, Map<URI, Object>> instances = new HashMap<>();

    <T> void registerInstance(URI primaryKey, T instance, URI context) {
        assert primaryKey != null;

        if (!instances.containsKey(context)) {
            instances.put(context, new HashMap<>());
        }
        instances.get(context).put(primaryKey, instance);
    }

    boolean containsInstance(URI primaryKey, URI context) {
        return instances.containsKey(context) && instances.get(context).containsKey(primaryKey);
    }

    Object getInstance(URI primaryKey, URI context) {
        if (instances.containsKey(context) && instances.get(context).containsKey(primaryKey)) {
            return instances.get(context).get(primaryKey);
        }
        return null;
    }

    void reset() {
        this.instances = new HashMap<>();
    }
}
