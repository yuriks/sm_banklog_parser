import {createMemo, createSignal, For, Show} from "solid-js";
import {toHex2, toHex4} from "./utils";
import classes from "./App.module.css";
import {createViewportObserver} from "@solid-primitives/intersection-observer";

function formatAddress(address) {
    if (address === null) {
        return "";
    }
    const bank = (address >> 16) & 0xFF;
    const low_addr = address & 0xFFFF;
    return `\$${toHex2(bank)}:${toHex4(low_addr)}:`
}

const Comment = () => <span class={classes.asm_comment}/>

const token_tag_types = {
    "Comment": Comment,
}

function renderLineContents(content, tokens) {
    let token_i = 0;
    let text_pos = 0;

    function inside_token(el, end_pos) {
        while (token_i < tokens.length && tokens[token_i].start < end_pos) {
            const next_token = tokens[token_i++];

            if (text_pos < next_token.start) {
                el.append(content.slice(text_pos, next_token.start));
                text_pos = next_token.start;
            }

            let new_el;
            if (Object.hasOwn(token_tag_types, next_token.t_type)) {
                new_el = token_tag_types[next_token.t_type]();
            } else {
                new_el = <span/>;
            }
            el.append(inside_token(new_el, next_token.end));
        }

        if (text_pos < end_pos) {
            el.append(content.slice(text_pos, end_pos));
            text_pos = end_pos;
        }

        return el;
    }

    return inside_token(<code/>, content.length);
}

function asmRow(line) {
    return (
        <>
            <Show when={line.prefix_lines}>
                <tr>
                    <td></td>
                    <td><code>{line.prefix_lines}</code></td>
                </tr>
            </Show>
            <tr>
                <td>{formatAddress(line.address)}</td>
                <td>{renderLineContents(line.content, line.tokens)}</td>
            </tr>
        </>
    );
}

function asmChunkSimple(chunk) {
    const pieces = [];
    for (const line of chunk) {
        if (line.prefix_lines) {
            pieces.push(line.prefix_lines);
        }
        pieces.push(formatAddress(line.address));
        pieces.push("   ");
        pieces.push(line.content);
    }

    return <pre textContent={pieces.join("")}/>;
}

function chunkLines(lines, chunkSize) {
    const chunks = [];
    for (let i = 0; i < lines.length; i += chunkSize) {
        chunks.push(lines.slice(i, i + chunkSize));
    }
    return chunks;
}

export function AsmView(props) {
    const chunks = createMemo(() => chunkLines(props.bankLines, 200));
    const [intersectionObserver] = createViewportObserver();

    return (
        <code class={classes.asmView}>
            <For each={chunks()}>{(chunk, i) => {
                const [visible, setVisible] = createSignal();
                return (
                    <div use:intersectionObserver={(ev) => setVisible(ev.isIntersecting)}>
                        <Show when={visible()} fallback={asmChunkSimple(chunk)}>
                            <table>
                                <colgroup>
                                    <col style={{"width": "12ch"}}/>
                                    <col/>
                                </colgroup>
                                <tbody>
                                <For each={chunk}>{asmRow}</For>
                                </tbody>
                            </table>
                        </Show>
                    </div>
                );
            }}</For>
        </code>
    );
}