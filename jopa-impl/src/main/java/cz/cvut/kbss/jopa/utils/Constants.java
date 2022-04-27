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
package cz.cvut.kbss.jopa.utils;

import cz.cvut.kbss.jopa.model.annotations.InheritanceType;

import java.net.URI;
import java.net.URL;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

/**
 * Declares some constants and default values used in JOPA.
 */
public class Constants {

    /**
     * Default inheritance type, used by the {@link cz.cvut.kbss.jopa.model.annotations.Inheritance} annotation.
     */
    public static final InheritanceType DEFAULT_INHERITANCE_TYPE = InheritanceType.TWO_STEP;

    private Constants() {
        throw new AssertionError();
    }
}
