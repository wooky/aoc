plugins {
    kotlin("multiplatform") version "1.4.10"
}

group = "ca.yakov"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
    jcenter()
}

kotlin {
    linuxX64 {
        compilations["main"].defaultSourceSet {
            kotlin.srcDir("src/main/kotlin")
        }
        binaries {
            sharedLib {

            }
        }
    }
    sourceSets {
        val linuxX64Main by getting {
            dependencies {
                implementation("com.soywiz.korlibs.krypto:krypto:2.0.2")
            }
        }
    }
}
