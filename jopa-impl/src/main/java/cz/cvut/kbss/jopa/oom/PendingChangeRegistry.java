/**
 * Copyright (C) 2011 Czech Technical University in Prague
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
