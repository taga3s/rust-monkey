import { useState } from "react";
import { Editor } from "@monaco-editor/react";
import styles from "./Code.module.css";
import init, { run } from "../../../scripts/pkg/runner";

const DEFAULT_CODE = `let a = "hello";
let b = "world!";
let concat = fn(x, y) { return x + y; };
concat(a, concat(" ", b));
`;

export const Code = () => {
	const [code, setCode] = useState(DEFAULT_CODE);
	const [result, setResult] = useState<string>("");

	const runCode = async (code: string) => {
		try {
			const _ = await init();
			const output = run(code) ?? "No output";
			setResult(output);
		} catch (err) {
			console.error(err);
		}
	};

	return (
		<div className={styles.container}>
			<div className={styles.editor}>
				<Editor
					value={code}
					onChange={(value) => setCode(value ?? "")}
					theme="vs-dark"
					options={{
						fontSize: 14,
						fontFamily: '"Fira Code", "Fira Mono", monospace',
						scrollBeyondLastLine: false,
						automaticLayout: true,
						minimap: { enabled: true },
						tabSize: 2,
					}}
				/>
			</div>
			<div className={styles.runner}>
				<div className={styles.runnerHeader}>
					<span>Results will be output here</span>
					<button
						onClick={() => {
							runCode(code);
						}}
					>
						Run code
					</button>
				</div>
				<div className={styles.runnerResult}>
					<ul>
						<li>&gt; {result}</li>
					</ul>
				</div>
			</div>
		</div>
	);
};
