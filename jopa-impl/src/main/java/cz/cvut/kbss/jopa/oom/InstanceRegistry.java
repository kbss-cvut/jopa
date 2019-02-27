/**
 * Copyright (C) 2019 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
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
