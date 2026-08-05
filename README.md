

# FITTEST

**FITTEST** (Framework for Integrated Testing of Enterprise Systems) is a research-driven suite of standalone tools designed for software instrumentation, execution logging, code coverage analysis, and model-based test generation. The repository provides modular components targeting Flex/Flash applications, PHP services, and Extended Finite State Machine (EFSM) model inference/transformation.

## 📦 Components

| Module | Description | Tech Stack |
| :--- | :--- | :--- |
| **ABCI** | ActionScript ByteCode Instrumenter. Injects `trace()` calls into AS3 methods for runtime monitoring. | Haskell (`asic` compiler) |
| **FCC** | Flash Code Coverage. Collects and aggregates line/branch coverage data from Flex applications. Includes a Maven plugin and coverage collector daemon. | Java, Maven, Flex SDK 4.6 |
| **PHP Logger** | Transparent HTTP traffic interceptor. Logs requests, responses, headers, and output to structured XML. | PHP |
| **EFSM2Selenium** | Translates Extended Finite State Machine models into executable Selenium WebDriver test suites. | Java, Freemarker |
| **Logs2EFSM** | Infers EFSM models from system execution logs for automated test case derivation. | Java |

## 🛠 Installation & Setup

### Prerequisites
- **Java JDK 8+** & **Maven 3+**
- **Haskell Platform** & **Cabal** (for ABCI)
- **Adobe Flex SDK 4.6** (for FCC/Flex targets)
- **GHC** (Glasgow Haskell Compiler)

### Building the Components
Each component is structured as an independent module. Navigate to the respective directory and run the build commands:

```bash
# 1. Flash Code Coverage (FCC)
cd "ITE Components standalone/Logging and Reporting Tools/Flash/FCC/v1"
mvn clean install

# 2. ABCI Instrumenter
cd "ITE Components standalone/Instrumenter Tools/abci"
cd lib && cabal install
cd ../compiler && cabal install

# 3. EFSM Tools
cd "Model  Inference and Transformer tools/EFSM-based/EFSM2Selenium"
mvn clean package
```

## 🚀 Usage Examples

### 1. Instrumenting a Flex Application (ABCI)
Use the provided script to pretty-print the AST, generate a Haskell support library, and inject instrumentation:
```bash
cd "ITE Components standalone/Instrumenter Tools/abci/examples/test-instr"
# Ensure your test app is compiled to Demo.swf first
bash run.sh
```

### 2. Flash Code Coverage Collection
Start the coverage collector daemon, pointing it to a coverage points file and a listening port:
```bash
# Start collector
java -cp coveragecollector/target/classes coveragecollector.CoverageCollector user 12345 path/to/coveragePointsClean.txt

# Aggregate raw reports (.rpw) after test execution
bash "ITE Components standalone/Logging and Reporting Tools/Flash/FCC/v1/coveragecollector/accumulator.sh"
```

### 3. PHP Request/Response Logging
Include the logger in your PHP entry point. It automatically registers output buffering and shutdown callbacks to capture full HTTP transactions:
```php
require_once 'ITE Components standalone/Logging and Reporting Tools/PHP/PHPLogger/v1/FittestPhpLogger.php';
// Logs are automatically saved to the configured directory as timestamped XML files
```

### 4. Generating Selenium Tests from EFSM
Transform a state machine model (`.efsm`) into a Java-based Selenium test suite:
```bash
cd "Model  Inference and Transformer tools/EFSM-based/EFSM2Selenium/src/main/examples/cart"
bash demo.sh
# Generates monitor code, compiles Java sources, and runs EvoSuite to produce the test suite
```

## 📁 Repository Structure
```
FITTEST/
├── ITE Components standalone/
│   ├── Instrumenter Tools/abci/          # Haskell-based AS3 instrumenter
│   ├── Logging and Reporting Tools/
│   │   ├── Flash/FCC/v1/                 # Flash Code Coverage (Maven multi-module)
│   │   └── PHP/PHPLogger/v1/             # PHP HTTP traffic logger
│   └── Model Inference and Transformer tools/
│       └── EFSM-based/                   # EFSM inference & Selenium generation
└── README.md
```

## 📜 License & Credits
This project was developed as part of academic and research initiatives (Universitat Politècnica de València, FBK-IRST, etc.). Components are provided as-is for research and educational purposes. Please refer to individual module headers for specific authorship and licensing details.
