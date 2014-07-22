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

package cz.cvut.kbss.jopa.owlapi;

import cz.cvut.kbss.jopa.model.metamodel.Attribute;

import java.util.Collection;
import java.util.HashSet;

public abstract class SimpleOneLevelCascadeExplorer extends OneLevelCascadeExplorer {

	protected void runForEach(final Attribute<?, ?> at, final Object o, boolean cascaded)
			throws IllegalAccessException {
		Object attVal = at.getJavaField().get(o);
		if (attVal == null) {
			return;
		}
		if (at.isCollection()) {
			for (final Object ox2 : new HashSet((Collection) attVal)) {
				if (cascaded) {
					runCascadedForEach(ox2);
				} else {
					runNonCascadedForEach(ox2);
				}
			}
		} else {
			if (cascaded) {
				runCascadedForEach(attVal);
			} else {
				runNonCascadedForEach(attVal);
			}
		}
	}

	protected void exploreCascaded(final Attribute<?, ?> at, final Object o)
			throws IllegalAccessException {
		runForEach(at, o, true);
	}

	protected void runCascadedForEach(Object ox2) {
		// nothing
	}

	protected void exploreNonCascaded(final Attribute<?, ?> at, final Object o)
			throws IllegalAccessException {
		runForEach(at, o, false);
	}

	protected void runNonCascadedForEach(Object ox2) {
		// nothing
	}
}