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
package cz.cvut.kbss.jopa.example03;

import java.io.File;

public class Environment {

    private Environment() {
        throw new AssertionError();
    }

    static void deleteRepositoryIfExists(String path) {
        assert path != null;

        final File repository = new File(path.substring("file:".length()));
        if (repository.exists()) {
            try {
                deleteDirectory(repository);
            } catch (IllegalArgumentException e) {
                System.err.println("Unable to delete repository. Exiting.");
                System.exit(2);
            }
        }
    }

    private static void deleteDirectory(File path) {
        File[] files = path.listFiles();
        if (files != null) {
            for (File f : files) {
                if (f.isDirectory()) {
                    deleteDirectory(f);
                } else {
                    if (!f.delete()) {
                        System.err.println("Unable to delete repository file " + f);
                        throw new IllegalArgumentException();
                    }
                }
            }
        }
        if (!path.delete()) {
            System.err.println("Unable to delete repository file " + path);
            throw new IllegalArgumentException();
        }
    }
}
