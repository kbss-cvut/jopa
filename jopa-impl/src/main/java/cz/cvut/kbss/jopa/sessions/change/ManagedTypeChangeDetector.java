/**
 * Copyright (C) 2016 Czech Technical University in Prague
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

import cz.cvut.kbss.jopa.sessions.MetamodelProvider;
import cz.cvut.kbss.jopa.utils.EntityPropertiesUtils;

public class ManagedTypeChangeDetector implements ChangeDetector {

    private final MetamodelProvider metamodelProvider;

    ManagedTypeChangeDetector(MetamodelProvider metamodelProvider) {
        this.metamodelProvider = metamodelProvider;
    }

    @Override
    public boolean hasChanges(Object clone, Object original) {
        final Object idOrig = EntityPropertiesUtils.getIdentifier(original, metamodelProvider.getMetamodel());
        final Object idClone = EntityPropertiesUtils.getIdentifier(clone, metamodelProvider.getMetamodel());

        if (idOrig == null && idClone != null || idOrig != null && idClone == null) {
            return true;
        }
        return idClone != null && !idClone.equals(idOrig);
    }
}
