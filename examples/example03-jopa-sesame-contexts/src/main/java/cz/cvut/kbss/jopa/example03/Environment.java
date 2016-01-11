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
