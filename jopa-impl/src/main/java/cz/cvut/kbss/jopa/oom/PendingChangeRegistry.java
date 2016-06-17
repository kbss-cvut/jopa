/**
 * Copyright (C) 2016 Czech Technical University in Prague
 * <p>
 * This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
 * version.
 * <p>
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License along with this program. If not, see
 * <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.oom;

import java.net.URI;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

class PendingChangeRegistry extends InstanceRegistry {

    void removeInstance(URI primaryKey, URI context) {
        final Map<URI, Object> contextInstances = instances.get(context);
        if (contextInstances != null) {
            contextInstances.remove(primaryKey);
        }
    }

    Map<URI, Map<URI, Object>> getInstances() {
        final Map<URI, Map<URI, Object>> result = new HashMap<>();
        instances.entrySet().stream().filter(entry -> !instances.get(entry.getKey()).isEmpty())
                 .forEach(entry -> result.put(entry.getKey(), Collections.unmodifiableMap(entry.getValue())));
        return result;
    }
}
