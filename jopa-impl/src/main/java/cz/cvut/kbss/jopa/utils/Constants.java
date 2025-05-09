/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.utils;

import cz.cvut.kbss.jopa.model.annotations.InheritanceType;

/**
 * Declares some constants and default values used in JOPA.
 */
public class Constants {

    /**
     * Default inheritance type, used by the {@link cz.cvut.kbss.jopa.model.annotations.Inheritance} annotation.
     */
    public static final InheritanceType DEFAULT_INHERITANCE_TYPE = InheritanceType.TWO_STEP;

    /**
     * Read-write transaction mode, used as the value for
     * {@link cz.cvut.kbss.jopa.model.JOPAPersistenceProperties#TRANSACTION_MODE}
     * <p>
     * Equals to "read_write"
     */
    public static final String READ_WRITE_TRANSACTION_MODE = "read_write";

    /**
     * Read-only transaction mode, used as the value for
     * {@link cz.cvut.kbss.jopa.model.JOPAPersistenceProperties#TRANSACTION_MODE}.
     * <p>
     * Equals to "read_only"
     */
    public static final String READ_ONLY_TRANSACTION_MODE = "read_only";

    private Constants() {
        throw new AssertionError();
    }
}
