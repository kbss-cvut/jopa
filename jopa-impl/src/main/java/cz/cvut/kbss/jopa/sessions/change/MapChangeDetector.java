/**
 * Copyright (C) 2022 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.sessions.change;

import java.util.Iterator;
import java.util.Map;

class MapChangeDetector implements ChangeDetector {

    private final ChangeDetector changeDetector;

    MapChangeDetector(ChangeDetector changeDetector) {
        this.changeDetector = changeDetector;
    }

    @Override
    public boolean hasChanges(Object clone, Object original) {
        assert clone != null;
        assert original != null;

        final Map<?, ?> cl = (Map<?, ?>) clone;
        final Map<?, ?> orig = (Map<?, ?>) original;
        if (orig.size() != cl.size()) {
            return true;
        }
        boolean changes = false;
        final Iterator<?> it = orig.keySet().iterator();
        while (it.hasNext() && !changes) {
            final Object origKey = it.next();
            if (!cl.containsKey(origKey)) {
                return true;
            }
            final Object origVal = orig.get(origKey);
            Object clVal = cl.get(origKey);

            changes = changeDetector.hasChanges(origVal, clVal);
        }
        return changes;
    }
}
